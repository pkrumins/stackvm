#!/usr/bin/env node

var express = require('express');
var Store = require('supermarket');
var Cart = require('cart');
var DNode = require('dnode');
var Hash = require('traverse/hash');

var User = require('./lib/models/user');
var web = require('./lib/web');

var port = Number(process.argv[2]) || 9000;

var app = express.createServer();
app.use(express.staticProvider(__dirname + '/static'));
app.use(express.cookieDecoder());
app.use(express.bodyDecoder());
app.use(express.session({
    store : new Cart({ dbFile : __dirname + '/data/sessions.db' }),
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

Store({
    filename : __dirname + '/data/users.db' //, json : true
}).all(function (err, names, metas) {
console.log('all!');
    var users = User.fromHashes(Hash.zip(names, metas));
    
    web(app, users);
    
    var Service = require('./lib/service');
    Service(users, function (service) {
        DNode(service)
            .listen(app, {
                ping : 1000,
                timeout : 1000,
                transports : 'websocket xhr-multipart xhr-polling htmlfile'
                    .split(/\s+/),
            })
            .listen(9090)
        ;
        app.listen(port, '0.0.0.0');
        console.log('StackVM running at http://localhost:' + port);
    });
});
