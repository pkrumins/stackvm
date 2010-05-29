// Connect to VMs over rfb

var sys = require('sys');
var fs = require('fs');
var base64_encode = require('base64').encode;
var spawn = require('child_process').spawn;

var Buffer = require('buffer').Buffer;
var PngLib = require('png');
var RFB = require('rfb').RFB;

var EventEmitter = require('events').EventEmitter;

VM.prototype = new EventEmitter;
exports.VM = VM;
function VM (opts) {
    var vm = this;
    var clients = {};
    var qemu;
    var rfb = new RFB(opts || {});
    rfb.addListener('error', function (msg) {
        vm.emit('error', {
            vm_id : opts.id,
            action : 'error',
            message : msg
        });
    });
    var pngStack;
    rfb.addListener('startRects', function (nRects) {
        if (nRects > 1) {
            pngStack = new PngLib.DynamicPngStack();
        }
    });
    rfb.addListener('endRects', function (nRects) {
        if (nRects > 1) {
            var png = pngStack.encode();
            var pngBuf = new Buffer(png.length);
            pngBuf.write(png, 'binary');

            var dims = pngStack.dimensions();
            vm.emit('png', {
                vm_id : opts.id,
                action : 'update_screen',
                png64 : base64_encode(pngBuf),
                width : dims.width,
                height : dims.height,
                x : dims.x,
                y : dims.y
            });
        }
    });
    rfb.addListener('raw', function (rect) {
        if (rect.nRects == 1) {
            var png = new PngLib.Png(rect.fb, rect.width, rect.height).encode();
            var pngBuf = new Buffer(png.length);
            pngBuf.write(png, 'binary');

            vm.emit('png', {
                vm_id : opts.id,
                action : 'update_screen',
                png64 : base64_encode(pngBuf),
                width : rect.width,
                height : rect.height,
                x : rect.x,
                y : rect.y
            });
        }
        else {
            pngStack.push(rect.fb, rect.x, rect.y, rect.width, rect.height);
        }
    });
    rfb.addListener('copyRect', function (rect) {
        vm.emit('copyRect', {
            vm_id : opts.id,
            action : 'copy_rect',
            width : rect.width,
            height : rect.height,
            dstX : rect.dstX,
            dstY : rect.dstY,
            srcX : rect.srcX,
            srcY : rect.srcY
        });
    });
    
    rfb.addListener('unknownRect', function (rect) {
        vm.emit('error', {
            vm_id : opts.id,
            action : 'error',
            message : 'received an unknownRect from rfb'
        });
    });
    
    this.attach = function (client) {
        clients[client] = {};
        clients[client]['send'] = function (msg) {
            client.send(JSON.stringify(msg));
        };
        this.addListener('error', clients[client]['send']);
        this.addListener('png', clients[client]['send']);
        this.addListener('copyRect', clients[client]['send']);

        clients[client]['send']({
            vm_id : opts.id,
            action : 'attached'
        });
    };
    
    this.detach = function (client) {
        this.removeListener('error', clients[client]['send']);
        this.removeListener('png', clients[client]['send']);
        this.removeListener('copyRect', clients[client]['send']);
        delete clients[client];
    };

    this.keyDown = function (client, key) {
        rfb.sendKeyDown(key);
    };

    this.keyUp = function (client, key) {
        rfb.sendKeyUp(key);
    };

    this.requestRedrawScreen = function (client) {
        rfb.requestRedrawScreen();
    };

    this.pointer = function (x, y, mask) {
        rfb.pointer(x, y, mask);
    };
}

