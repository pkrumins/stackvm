#!/usr/bin/env node
// This program serves stackvm webpages and stackvm streams.
require.paths.unshift(__dirname + '/..');

var sys = require('sys');
var socketio = require('socket.io');

var webserver = require('lib/webserver').webserver;
var VM = require('lib/vm').VM;

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

socketio.listen(webserver, {
    // flashsockets require that pesky 843 port, so forget those
    transports : 'websocket htmlfile xhr-multipart xhr-polling'.split(/\s+/),
    
    onClientConnect : function(client) {
        var client_ip = client.request.connection.remoteAddress;
        sys.log("Client from " + client_ip + " connected.");
    },
    
    onClientDisconnect : function(client) {
        var client_ip = client.request.connection.remoteAddress;
        sys.log("Client from " + client_ip + " disconnected.");
        vms['linux'].stop(); // for now
    },
    
    onClientMessage : function(msg, client) {
        var vm = vms[msg.vm_id];
        sys.log('action: ' + msg.action);
        var f = {
            list : function () {
                var acc = [];
                for (var id in vms) acc.push(vms[id]);
                client.send(acc);
            },
            status : function () {
                client.send(vm.status);
            },
            start : function () {
                vm.start();
                vm.attach(client);
                client.send(JSON.stringify({
                    vm_id : msg.vm_id,
                    action : 'connected'
                }));
            },
            stop : function () {
                vm.stop();
                vm.detach(client);
            },
            attach : function () {
                vm.attach(client);
            },
            detach : function () {
                vm.detach(client);
            },
            key_down : function () {
                vm.keyDown(client, parseInt(msg.key, 10));
            },
            key_up : function () {
                vm.keyUp(client, parseInt(msg.key, 10));
            },
            redraw_screen : function () {
                vm.requestRedrawScreen(client);
            }
        }[msg.action];
        if (f) f();
    }
});

