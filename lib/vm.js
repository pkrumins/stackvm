// Connect to VMs over rfb

var sys = require('sys');
var fs = require('fs');
var Base64 = require('base64');
var spawn = require('child_process').spawn;

var Png = require('png').Png;
var RFB = require('rfb').RFB;

var EventEmitter = require('events').EventEmitter;

VM.prototype = new EventEmitter;
exports.VM = VM;
function VM (opts) {
    var vm = this;
    var clients = {};
    var qemu;
    var rfb;
    
    this.status = 'stopped';
    
    this.start = function () {
        qemu = spawn(
            'qemu',
            [ '-vnc', 'localhost:' + (opts.port - 5900), opts.image ]
        );
        this.status = 'running';
        
        qemu.addListener('exit', function (code, signal) {
            this.status = 'stopped';
            rfb.removeListener('raw', raw);
        });
        
        rfb = new RFB(opts || {});
        rfb.addListener('raw', function (raw) {
            var png = new Png(raw.fb, raw.width, raw.height);
            vm.emit('png', {
                png64 : Base64.encode(png.encode()),
                width : raw.width,
                height : raw.height,
                x : raw.x,
                y : raw.y,
            });
        });
    };
    
    this.stop = function () {
        if (qemu) qemu.kill('SIGHUP');
    };
    
    this.restart = function () {
        qemu.stop();
        qemu.start();
    };
    
    this.attach = function (client) {
        clients[client] = function (png) {
            client.write(png);
        };
        this.addListener('png', clients[client]);
    };
    
    this.detach = function (client) {
        this.removeListener('png', clients[client]);
    };
}

