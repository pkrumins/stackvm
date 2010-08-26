#!/usr/bin/env node
// This program serves stackvm webpages and stackvm streams.
require.paths.unshift(__dirname + '/../..');

var fs = require('fs');
var connect = require('connect');
var DNode = require('dnode');

var Remote = require('lib/remote');
var User = require('lib/models/user');

var port = Number(process.argv[2]) || 9001;

var dnodeJS = require('dnode/web').source();

var webserver = connect.createServer(
    connect.staticProvider(__dirname + '/../../static/api'),
    function (req, res) {
        if (req.url == '/dnode.js') {
            res.writeHead(200, { 'Content-Type' : 'text/javascript' });
            res.end(dnodeJS)
        }
    }
).listen(port, '0.0.0.0');
console.log('Connect server listening on port ' + port);

var users = User.fromBatch(JSON.parse(
    fs.readFileSync(__dirname + '/../../data/users.json', 'ascii')
));

cookieSessions = {
    'xyzzypkrumins' : { user: 'pkrumins' },
    'xyzzysubstack' : { user: 'substack' },
};

//DNode.connect(9077, function (manager) {
    DNode(function (client, conn) {
        this.authenticate = function (params, cb) {
            if ('cookie' in params) {
                var cookieData = cookieSessions[params.cookie];
                if (cookieData) {
                    cb(Remote.attach(conn, users[cookieData.user]));
                }
                else {
                    cb(null);
                }
            }
            else {
                if (params.name == 'anonymous') {
                    // no password for anonymous
                    cb(Remote.attach(conn, users.anonymous));
                }
                else {
                    users[params.name].authenticate(pass, function (user) {
                        cb(Remote.attach(conn, user));
                    });
                }
            }
        };
    }).listen({
        protocol : 'socket.io',
        server : webserver,
        transports : 'websocket xhr-multipart xhr-polling htmlfile'
            .split(/\s+/),
    }).listen(9002);
//});
