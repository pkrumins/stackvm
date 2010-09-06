#!/usr/bin/env node
// This program serves stackvm webpages and stackvm streams.
require.paths.unshift(__dirname + '/..');
require.paths.unshift(__dirname + '/../lib');

var sys = require('sys');
var fs = require('fs');
var crypto = require('crypto');

var DNode = require('dnode');
var Hash = require('traverse/hash');

var port = Number(process.argv[2]) || 9000;
var webserver = require('lib/webserver').webserver;
webserver.listen(port, '0.0.0.0');
console.log('Webserver running at http://localhost:' + port);

var Session = require('lib/session');
var User = require('models/user');

var cookies = {};

var jsonFile = __dirname + '/../data/users.json';
var json = JSON.parse(fs.readFileSync(jsonFile));
var users = Hash.map(json, function (user, name) {
    user.processes = {};
    user.name = name;
    return user;
});

DNode(function (client,conn) {
    this.authenticate = function (params, cb) {
        if (params.user && params.pass) {
            var hash = User.hash(params.pass);
            var name = params.user.toLowerCase();
            var user = users[name];
            if (users.hasOwnProperty(name) && hash == user.hash) {
                var session = new Session({
                    client : client,
                    connection : conn,
                    user : user,
                });
                cb(session);
            }
            else {
                cb(null);
            }
        }
    };
}).listen({
    server : webserver,
    transports : 'websocket xhr-multipart xhr-polling htmlfile'.split(/\s+/),
});
