/*
** An XMPP component that handles communications between browser and stackvm.
*/

var sys  = require('sys');
var tcp  = require('net');
var http = require('http');
var buffer = require('buffer');

var xmpp = require('./libs/xmpp');
var base64 = require('./libs/base64');

var xmpp_jid  = 'vm.localhost';
var xmpp_pass = 'password';
var xmpp_host = 'localhost';
var xmpp_port = 5347;             // default prosody port

var vm_map = {
  linux1: {'host': '127.0.0.1', 'port': 25900},
  dos1:   {'host': '127.0.0.1', 'port': 25901}
};

function xmpp_connect_cb(status, error_message) {
  xmpp_server = "XMPP server at " + xmpp_host + ":" + xmpp_port;
  action_table = {};
  action_table[xmpp.Status.CONNECTED] = function (_) {
    sys.log("Connected to " + xmpp_server);
    xmpp_connection.addHandler(onMessage, null, 'message');
  };
  action_table[xmpp.Status.CONNECTING] = function (_) {
    sys.log("Connecting to " + xmpp_server);
  };
  action_table[xmpp.Status.CONNFAIL] = function(err) {
    sys.log("Failed connecting to " + xmpp_server + ". Error: " + err);
  };
  action_table[xmpp.Status.DISCONNECTING] = function(_) {
    sys.log("Disconnecting from " + xmpp_server);
  };
  action_table[xmpp.Status.DISCONNECTED] = function(_) {
    sys.log("Disconnected from " + xmpp_server);
  };
  action_table[xmpp.Status.AUTHENTICATING] = function(_) {
    sys.log("Authenticating at " + xmpp_server);
  };
  action_table[xmpp.Status.AUTHFAIL] = function(_) {
    sys.log("Failed authenticating at " + xmpp_server);
  };
  cb = action_table[status];
  if (!cb) {
    sys.log("No action handler for action '" + status + "' was not found!");
    return;
  }
  action_table[status](error_message);
}

var stackvms = {};

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
    res.addListener('data', function(chunk) {
      data.write(chunk, encoding);
    });
    res.addListener('end', function() {
      callback && callback(data, res);
    });
  });
  req.end();
}

function handler_start_vm(vm_id, msg) {
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

  xmpp_connection.send(
    xmpp.message({to:msg.getAttribute('from')}).
      c('vm_id').t(vm_id).up().
      c('action').t('connected')
  );

  GetWithCallback(
    vm_map[vm_id]['host'], vm_map[vm_id]['port'],
    '/api/console/get_screen',
    function(png, response) {
      xmpp_connection.send(xmpp.message({to:msg.getAttribute('from')}).
        c('vm_id').t(vm_id).up().
        c('action').t('redraw_screen').up().
        c('width').t(response.headers['screen-width']).up().
        c('height').t(response.headers['screen-height']).up().
        c('png').t(base64.encode(png))
      );
    }
  );

  update_fetcher(0, vm_id, msg);
}

function update_fetcher(version_id, vm_id, msg) {
  GetWithCallback(
    vm_map[vm_id]['host'], vm_map[vm_id]['port'],
    '/api/console/get_update_list/' + version_id,
    function(updates, response) {
      updates = JSON.parse(updates.toString());
      var latest_version = updates[0][0];
      for (var i = 1; i < updates.length; i++) {
        var item = updates[i];
        push_update(vm_id, msg, item, version_id, i-1);
      }
      update_fetcher(latest_version, vm_id, msg);
    },
    'ascii'
  );
}

function push_update(vm_id, msg, item, version_id, update_id) {
  GetWithCallback(
    vm_map[vm_id]['host'], vm_map[vm_id]['port'],
    '/api/console/get_update/' + version_id + '/' + update_id,
    function(png, response) {
      xmpp_connection.send(xmpp.message({to:msg.getAttribute('from')}).
        c('vm_id').t(vm_id).up().
        c('action').t('update_screen').up().
        c('x').t(item[0]).up().
        c('y').t(item[1]).up().
        c('width').t(item[2]).up().
        c('height').t(item[3]).up().
        c('png').t(base64.encode(png))
      );
    }
  );
}

function handler_key_down(vm_id, msg) {
  var key = msg.getChild('key').getText();
  Get(
    vm_map[vm_id]['host'], vm_map[vm_id]['port'],
    '/api/console/send_key_down/' + key
  );
}

function handler_key_up(vm_id, msg) {
  var key = msg.getChild('key').getText();
  Get(
    vm_map[vm_id]['host'], vm_map[vm_id]['port'],
    '/api/console/send_key_up/' + key
  );
}

var handlers = {
  'start_vm': function(vm_id, msg) { handler_start_vm(vm_id, msg); },
  'key_down': function(vm_id, msg) { handler_key_down(vm_id, msg); },
  'key_up':   function(vm_id, msg) { handler_key_up(vm_id, msg); },
}

function onMessage(msg) {
  var vm_id = msg.getChild('vm_id');
  if (!vm_id) {
    sys.log("Unknown message: ");
    sys.log(msg.toString());
    return;
  }
  vm_id = vm_id.getText();
  if (!vm_map[vm_id]) {
    sys.log("Unknown VM ID: " + vm_id);
    sys.log(msg.toString());
    xmpp_connection.send(
      xmpp.message({to:msg.getAttribute('from')}).
      c('vm_id').t(vm_id).up().
      c('error').t("There is no VM with id '" + vm_id + "'")
    );
    return;
  }
  action = msg.getChild('action');
  if (!action) {
    sys.log("Unknown action for VM ID: " + vm_id);
    sys.log(msg.toString());
    xmpp_connection.send(
      xmpp_message({to:msg.getAttribute('from')}).
      c('vm_id').t(vm_id).up().
      c('error').t("No action specified for VM with id '" + vm_id + "'")
    );
    return;
  }
  action = action.getText();
  handler = handlers[action];
  if (!handler) {
    sys.log("Unknown action " + action + " for VM ID: " + vm_id);
    sys.log(msg.toString());
    return;
  }
  handler(vm_id, msg);
}

var xmpp_connection = new xmpp.Connection(xmpp_host, xmpp_port);
xmpp_connection.connect(xmpp_jid, xmpp_pass, xmpp_connect_cb);

