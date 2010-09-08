#!/usr/bin/env node
// This program serves stackvm webpages and stackvm streams.

require.paths.unshift(__dirname + '/.');
require.paths.unshift(__dirname + '/lib');

var port = Number(process.argv[2]) || 9000;

var express = require('express');
var app = express.createServer();
app.use(express.staticProvider(__dirname + '/static'));
app.get('/js/dnode.js', require('dnode/web').route());

app.listen(port, '0.0.0.0');
console.log('Webserver running at http://localhost:' + port);

var Session = require('lib/session');
var User = require('models/user');

var cookies = {};

// todo: real database goes here
var jsonFile = __dirname + '/data/users.json';
var json = JSON.parse(require('fs').readFileSync(jsonFile));
var users = User.fromHashes(json);

var DNode = require('dnode');
DNode(function (client, conn) {
    this.authenticate = function (params, cb) {
        if (params.user && params.pass) {
            var hash = User.hash(params.pass);
            var name = params.user.toLowerCase();
            var user = users[name];
            if (users.hasOwnProperty(name) && hash == user.hash) {
                var session = new Session(user, conn);
                user.sessions[conn.id] = session;
                
                if (user.connections == 0) user.emit('online');
                user.connections ++;
                
                conn.on('end', function () {
                    user.connections --;
                    delete user.sessions[conn.id];
                    if (user.connections == 0) user.emit('offline');
                });
                
                cb(session.attach(conn));
            }
            else {
                cb(null);
            }
        }
    };
}).listen({
    server : app,
    transports : 'websocket xhr-multipart xhr-polling htmlfile'.split(/\s+/),
});
