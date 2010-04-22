/*
** A component that communicates with stackvm software.
*/

var sys  = require('sys');
var xmpp = require('./libs/xmpp');

var xmpp_jid  = 'vm.localhost';
var xmpp_pass = 'password';
var xmpp_host = 'localhost';
var xmpp_port = 5347;             // default prosody port

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

function onMessage(msg) {
  var vm_id = msg.getChild().getText();
  sys.log(vm_id);
}

var xmpp_connection = new xmpp.Connection(xmpp_host, xmpp_port);
xmpp_connection.connect(xmpp_jid, xmpp_pass, xmpp_connect_cb);

