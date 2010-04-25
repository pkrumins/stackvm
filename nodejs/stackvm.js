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

  var vm  = http.createClient(vm_map[vm_id]['port'], vm_map[vm_id]['host']);
  var req = vm.request('GET', '/api/console/get_screen');
  req.addListener('response', function(res) {
    res.setEncoding('binary');
    var png = new buffer.Buffer(parseInt(res.headers['content-length']));
    res.addListener('data', function (chunk) {
      png.write(chunk, 'binary');
    });
    res.addListener('end', function () {
      xmpp_connection.send(xmpp.message({to:msg.getAttribute('from')}).
      c('vm_id').t(vm_id).up().
      c('action').t('redraw_screen').up().
      c('png').t(base64.encode(png)));
    });
  });
  req.end();
}

function handler_key_down(vm_id, msg) {
  var key = msg.getChild('key').getText();
  sys.log('got: ' + key);
  var vm  = http.createClient(vm_map[vm_id]['port'], vm_map[vm_id]['host']);
  var req = vm.request('GET', '/api/console/send_key_down/' + key);
  req.end();
}

function handler_key_up(vm_id, msg) {
  var key = msg.getChild('key').getText();
  var vm  = http.createClient(vm_map[vm_id]['port'], vm_map[vm_id]['host']);
  var req = vm.request('GET', '/api/console/send_key_up/' + key);
  req.end();
}

var handlers = {
  'start_vm': function(vm_id, msg) { handler_start_vm(vm_id, msg); },
  'key_down': function(vm_id, msg) { handler_key_down(vm_id, msg); },
  'key_up':   function(vm_id, msg) { handler_key_up(vm_id, msg); },
}

function onMessage(msg) {
  var vm_id = msg.getChild('vm_id');
  if (vm_id) {
    vm_id = vm_id.getText();
    if (!vm_map[vm_id]) {
      xmpp_connection.send(
        xmpp.message({to:msg.getAttribute('from')}).
        c('vm_id').t(vm_id).up().
        c('error').t("There is no VM with id '" + vm_id + "'"));
      return;
    }
    action = msg.getChild('action').getText();
    handler = handlers[action];
    if (!handler) {
      sys.log("Unknown action '" + action + "' for message:");
      sys.log(msg.toString());
      return;
    }
    handler(vm_id, msg);
  }
  else {
    sys.log("Unknown message: ");
    sys.log(msg.toString());
  }
}

var xmpp_connection = new xmpp.Connection(xmpp_host, xmpp_port);
xmpp_connection.connect(xmpp_jid, xmpp_pass, xmpp_connect_cb);

