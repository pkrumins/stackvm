#!/usr/bin/env node

var express = require('express');
var DNode = require('dnode');
var Hash = require('traverse/hash');
var Cart = require('cart');
var fs = require('fs');

var User = require('../lib/models/user');
var Web = require('../lib/web');
var Service = require('../lib/service');

var argv = require('optimist').argv;
var port = parseInt(argv._[0], 10) || 9000;

var app = express.createServer();
app.use(express.staticProvider(__dirname + '/../static'));
app.use(express.cookieDecoder());
app.use(express.bodyDecoder());
app.use(express.session({
    store : new Cart({ dbFile : process.cwd() + '/data/sessions.db' }),
    secret : 'todo: set this in the stackvm site config with cli.js'
}));

app.configure('development', function () {
    app.use(express.errorHandler({ 
        dumpExceptions: true,
        showStack: true 
    }));
});

app.configure('production', function () {
    app.use(express.errorHandler());
});

app.get('/js/dnode.js', require('dnode/web').route());

// TODO: use supermarket here
var users = User.fromHashes(
    JSON.parse(fs.readFileSync(process.cwd() + '/data/users.json'))
);

Web(app, users);
Service(users, function (service) {
    DNode(service)
        .listen(app, {
            ping : 3600*1000,
            timeout : 1000,
            transports : 'websocket xhr-multipart xhr-polling htmlfile'
                .split(/\s+/),
        })
        .listen(9090)
    ;
    console.log('StackVM running at http://localhost:' + port);
});

app.listen(port, '0.0.0.0');
