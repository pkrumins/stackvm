#!/usr/bin/env node
// This program serves stackvm webpages and stackvm streams.
require.paths.unshift(__dirname + '/..');

var sys = require('sys');
var fs = require('fs');
var DNode = require('dnode').DNode;
var crypto = require('crypto');

var webserver = require('lib/webserver').webserver;
var Session = require('lib/session');

var port = Number(process.argv[2]) || 9000;

webserver.listen(port, '0.0.0.0');
sys.log("Webserver running at 0.0.0.0:" + port + ".");

DNode.connect(9077, function (manager) {
    DNode(function (client,conn) {
        this.authenticate = function (name,pass,cb) {
            manager.authenticate(name, pass, function (user) {
                if (user) {
                    cb(new Session({
                        client : client,
                        connection : conn,
                        user : user,
                        manager : manager,
                    }));
                }
                else cb(null);
            });
        };
    }).listen({
        protocol : 'socket.io',
        server : webserver,
        transports : 'websocket xhr-multipart xhr-polling htmlfile'
            .split(/\s+/),
    }).listen(9001);
});
