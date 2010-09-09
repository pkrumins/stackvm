#!/usr/bin/env node

var express = require('express');
var nStore = require('nStore');
var DNode = require('dnode');
var Hash = require('traverse/hash');

var port = Number(process.argv[2]) || 9000;

require.paths.unshift(__dirname + '/.');
require.paths.unshift(__dirname + '/lib');

var Service = require('lib/service');
var User = require('models/user');

var sessions = nStore(__dirname + '/data/sessions.db');

var app = express.createServer();
app.use(express.staticProvider(__dirname + '/static'));
app.get('/js/dnode.js', require('dnode/web').route());
app.listen(port, '0.0.0.0');

nStore(__dirname + '/data/users.db').all(
    function () { return true },
    function (err, hashes, metas) {
        var users = User.fromHashes(Hash.zip(metas, hashes));
console.log('== users ==');
console.dir(users);
        var service = Service(users);
        DNode(service).listen({
            server : app,
            transports : 'websocket xhr-multipart xhr-polling htmlfile'
                .split(/\s+/),
        });
            console.log('Webserver running at http://localhost:' + port);
        
    }
);

/*
var fs = require('fs');
var json = fs.readFileSync(__dirname + '/data/users.json');
var users = User.fromHashes(JSON.parse(json));
*/

