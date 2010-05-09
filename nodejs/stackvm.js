#!/usr/bin/env node
// Stackvm mediator.

var sys  = require('sys');
var http = require('http');
var base64 = require('./libs/base64');
var buffer = require('buffer');

// for now just a map, later lookup from database, etc.
var vm_map = {
  linux1: {'host': '127.0.0.1', 'port': 25900},
  dos1:   {'host': '127.0.0.1', 'port': 25901}
};

function Get(host, port, url) {
  var vm  = http.createClient(port, host);
  var req = vm.request('GET', url);
  req.end();
}

function GetWithCallback(host, port, url, callback, encoding) {
  var client = http.createClient(port, host);
  var req    = client.request('GET', url);
  var encoding = encoding||'binary';
  req.addListener('response', function(res) {
    res.setEncoding(encoding);
    var data = new buffer.Buffer(parseInt(res.headers['content-length']));
    var offset = 0;
    res.addListener('data', function(chunk) {
      var written = data.write(chunk, encoding, offset);
      offset += written;
    });
    res.addListener('end', function() {
      callback && callback(data, res);
    });
  });
  req.end();
}

function handler_start_vm(msg, client) {
  /*
  ** TODO: This when we have a permanent TCP connections with stackvm
  *
  stackvm_socket = net.createConnection(vm_map[vm_id]['port'], vm_map[vm_id]['host']);
  stackvm_socket.setTimeout(0);
  stackvm_socket.addListener("connect", function() {
    xmpp.message({to:msg.getAttribute('from')}).
    c('vm_id').t(vm_id).up().
    c('action').t('connect');
  });
  stackvm_socket.addListener("disconnect", function(errno) {
    xmpp.message({to:msg.getAttribute('from')}).
    c('vm_id').t(vm_id).up().
    c('action').t('disconnect');
  });
  // stackvm_socket.addListener("data", function(data) { });
  */

  /*
  ** But for now, let's use the same logic as in console.js
  */

  client.send(JSON.stringify({
    vm_id:  msg.vm_id,
    action: 'connected'
  }));

  handler_redraw_screen(msg, client);

  update_fetcher(0, msg, client);
}

function update_fetcher(version_id, msg, client) {
  GetWithCallback(
    vm_map[msg.vm_id]['host'], vm_map[msg.vm_id]['port'],
    '/api/console/get_update_list/' + version_id,
    function(updates, response) {
      updates = JSON.parse(updates.toString());
      var latest_version = updates[0][0];
      for (var i = 1; i < updates.length; i++) {
        var item = updates[i];
        push_update(msg, client, item, version_id, i-1);
      }
      update_fetcher(latest_version, msg, client);
    },
    'ascii'
  );
}

function push_update(msg, client, item, version_id, update_id) {
  GetWithCallback(
    vm_map[msg.vm_id]['host'], vm_map[msg.vm_id]['port'],
    '/api/console/get_update_base64/' + version_id + '/' + update_id,
    function(png, response) {
      client.send(JSON.stringify({
        vm_id:   msg.vm_id,
        action:  'update_screen',
        x:       item[0],
        y:       item[1],
        width:   item[2],
        height:  item[3],
        png:     png.toString('ascii')
      }));
    },
    'ascii'
  );
}

function handler_key_down(msg, client) {
  Get(
    vm_map[msg.vm_id]['host'], vm_map[msg.vm_id]['port'],
    '/api/console/send_key_down/' + msg.key
  );
}

function handler_key_up(msg, client) {
  Get(
    vm_map[msg.vm_id]['host'], vm_map[msg.vm_id]['port'],
    '/api/console/send_key_up/' + msg.key
  );
}

function handler_pointer(msg, client) {
  Get(
    vm_map[msg.vm_id]['host'], vm_map[msg.vm_id]['port'],
    '/api/console/send_pointer/[' + [msg.x, msg.y, msg.mask].join(',') + ']'
  );
}

function handler_redraw_screen(msg, client) {
  GetWithCallback(
    vm_map[msg.vm_id]['host'], vm_map[msg.vm_id]['port'],
    '/api/console/get_screen_base64',
    function(png, response) {
      client.send(JSON.stringify({
        vm_id:  msg.vm_id,
        action: 'redraw_screen',
        width:  response.headers['screen-width'],
        height: response.headers['screen-height'],
        png:    png.toString('ascii')
      }));
    },
    'ascii'
  );
}

// TODO: just call handler_*(msg, client) for the given action
var handlers = {
  'start_vm': function(msg, client) { handler_start_vm(msg, client); },
  'redraw_screen': function(msg, client) { handler_redraw_screen(msg, client); },
  'key_down': function(msg, client) { handler_key_down(msg, client); },
  'key_up': function(msg, client) { handler_key_up(msg, client); },
  'pointer': function(msg, client) { handler_pointer(msg, client); },
}

function handle_message(msg, client) {
  if (!msg.vm_id) {
    sys.log("Unknown message:");
    sys.log(JSON.stringify(msg));
    return;
  }
  if (!vm_map[msg.vm_id]) {
    sys.log("Unknown VM ID:");
    sys.log(JSON.stringify(msg));
    client.send(JSON.stringify({
      error: "There is no VM with id '" + msg.vm_id + "'"
    }));
    return;
  }
  if (!msg.action) {
    sys.log("Unknown action:");
    sys.log(JSON.stringify(msg));
    client.send(JSON.stringify({
      error: "No action specified."
    }));
    return;
  }
  handler = handlers[msg.action];
  if (!handler) {
    sys.log("Unknown handler.");
    sys.log(JSON.stringify(msg));
    return;
  }
  handler(msg, client);
}

exports.handle_message = handle_message;

