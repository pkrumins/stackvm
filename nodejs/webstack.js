#!/home/substack/prefix/bin/node
// This program serves stackvm webpages and stackvm streams.

var sys       = require('sys');
var webserver = require('./webserver').webserver;
var socketio  = require('./libs/socket.io');
var stackvm   = require('./stackvm');

var port = Number(process.argv[2]) || 9000;

webserver.listen(port, '0.0.0.0');
sys.log("Webserver running at 0.0.0.0:" + port + ".");

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

