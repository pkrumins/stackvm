#!/usr/bin/env node
// This program serves stackvm webpages and stackvm streams.
require.paths.unshift(__dirname + '/..');

var sys = require('sys');
var fs = require('fs');
var DNode = require('dnode').DNode;
var crypto = require('crypto');

var webserver = require('lib/webserver').webserver;
var Session = require('lib/session').Session;

var port = Number(process.argv[2]) || 9000;

webserver.listen(port, '0.0.0.0');
sys.log("Webserver running at 0.0.0.0:" + port + ".");

DNode.connect(9077, function (manager) {
    DNode(function (client,conn) {
        this.authenticate = function (user,pass,cb) {
            if (/[^\w-]/.test(user)) {
                cb(null);
                return;
            }
            var passFile = __dirname + '/../users/' + user + '/passwd';
            var hash = new crypto.Hash('sha512').update(pass).digest('hex');
            fs.readFile(passFile, function (err,fileHash) {
                cb(hash == fileHash
                    ? new Session({
                        client : client,
                        connection : conn,
                        user : user,
                        manager : manager,
                    })
                    : null
                );
            });
        };
    }).listen({
        protocol : 'socket.io',
        server : webserver,
        transports : 'websocket xhr-multipart xhr-polling htmlfile'
            .split(/\s+/),
    }).listen(9001);
});
