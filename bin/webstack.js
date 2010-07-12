#!/usr/bin/env node
// This program serves stackvm webpages and stackvm streams.
require.paths.unshift(__dirname + '/..');

var sys = require('sys');
var webserver = require('lib/webserver').webserver;
var VM = require('lib/vm').VM;
var DNode = require('dnode').DNode;

var port = Number(process.argv[2]) || 9000;

var vms = {
    linux : new VM({
        id : 'linux',
        image : __dirname + '/../vms/linux-0.2.img',
        port : 5900,
    }),
};

webserver.listen(port, '0.0.0.0');
sys.log("Webserver running at 0.0.0.0:" + port + ".");

DNode(function (client) {
    keyDown : function (key) {
        vm.keyDown(key);
    },
    keyUp : function (key) {
        vm.keyUp(key);
    },
    pointer : function (x, y, mask) {
        vm.sendPointer(x, y, mask);
    },
    redrawScreen : function () {
        vm.requestRedrawScreen();
    },
    takeScreenshot : function () {
        vm.takeScreenshot();
    },
    startScreencast : function () {
        vm.startScreencast();
    },
    stopScreencast : function () {
        vm.stopScreencast();
    },
    attach : function () {
        vm.attach(client);
        vm.requestRedrawScreen();
    },
    detach : function () {
        vm.detach(client);
    }
}).listen({
    protocol : 'socket.io',
    server : webserver,
    transports : 'websocket xhr-multipart xhr-polling htmlfile'.split(/\s+/),
});

