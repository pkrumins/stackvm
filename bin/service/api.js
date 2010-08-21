#!/usr/bin/env node
// This program serves stackvm webpages and stackvm streams.
require.paths.unshift(__dirname + '/../..');

var fs = require('fs');
var connect = require('connect');
var DNode = require('dnode');

var Remote = require('lib/remote');
var User = require('lib/models/user');

var port = Number(process.argv[2]) || 9000;

var webserver = connect.createServer(
    connect.staticProvider(__dirname + '/../../static/api')
).listen(port, '0.0.0.0');
console.log('Connect server listening on port ' + port);

var users = User.fromList(JSON.parse(
    fs.readFileSync(__dirname + '/../../data/users.json', 'ascii')
));

//DNode.connect(9077, function (manager) {
    DNode(function (client, conn) {
        this.authenticate = function (name, pass, cb) {
            users[name].authenticate(pass, function (user) {
                cb(Remote.attach(conn, user));
            });
        };
    }).listen({
        protocol : 'socket.io',
        server : webserver,
        transports : 'websocket xhr-multipart xhr-polling htmlfile'
            .split(/\s+/),
    }).listen(9001);
//});
