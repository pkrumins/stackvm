var sys       = require('sys');
var webserver = require('./webserver').webserver;
var socketio  = require('./libs/socket.io');
var stackvm   = require('./stackvm');

webserver.listen(9000, '0.0.0.0');
sys.log("Webserver running at 0.0.0.0:9000.");

socketio.listen(webserver, {
  onClientConnect: function(client) {
    var client_ip = client.request.connection.remoteAddress;
    sys.log("Client from " + client_ip + " connected.");
  },

  onClientDisconnect: function(client) {
    var client_ip = client.request.connection.remoteAddress;
    sys.log("Client from " + client_ip + " disconnected.");
  },

  onClientMessage: function(msg, client) {
    stackvm.handle_message(msg, client);
  }
});

