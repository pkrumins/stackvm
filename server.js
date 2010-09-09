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

var app = express.createServer();
app.use(express.staticProvider(__dirname + '/static'));
app.get('/js/dnode.js', require('dnode/web').route());
app.listen(port, '0.0.0.0');

var sessions = nStore(__dirname + '/data/sessions.db');

nStore(__dirname + '/data/users.db').all(
    function () { return true },
    function (err, hashes, metas) {
        var keys = metas.map(function (x) { return x.key });
        var users = User.fromHashes(Hash.zip(keys, hashes));
        var service = Service(users);
        DNode(service).listen({
            server : app,
            transports : 'websocket xhr-multipart xhr-polling htmlfile'
                .split(/\s+/),
        });
        console.log('StackVM running at http://localhost:' + port);
    }
);
