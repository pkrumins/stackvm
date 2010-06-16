// Connect to VMs over rfb

var sys = require('sys');
var fs = require('fs');
var base64_encode = require('base64').encode;
var spawn = require('child_process').spawn;

var Buffer = require('buffer').Buffer;
var PngLib = require('png');
var RFB = require('rfb').RFB;
var DNode = require('dnode').DNode;

var EventEmitter = require('events').EventEmitter;

VM.prototype = new EventEmitter;
exports.VM = VM;
function VM (opts) {
    var vm = this;
    var clients = {};
    var pngStack = null;
    var screenShotRequests = 0;
    var rfb = new RFB(opts || {});

    rfb.addListener('error', function (msg) {
        vm.emit('error', {
            message : msg
        });
    });

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
                image : pngBuf,
                image64 : base64_encode(pngBuf),
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

            var rfbDims = rfb.fbDims();
            var fullScreen = (rect.width == rfbDims.width) &&
                (rect.height == rfbDims.height);

            vm.emit('png', {
                image : pngBuf,
                image64 : base64_encode(pngBuf),
                width : rect.width,
                height : rect.height,
                x : rect.x,
                y : rect.y,
                fullScreen : fullScreen
            });
        }
        else {
            pngStack.push(rect.fb, rect.x, rect.y, rect.width, rect.height);
        }
    });

    rfb.addListener('copyRect', function (rect) {
        vm.emit('copyRect', {
            width : rect.width,
            height : rect.height,
            dstX : rect.x,
            dstY : rect.y,
            srcX : rect.srcX,
            srcY : rect.srcY
        });
    });

    rfb.addListener('desktopSize', function (rect) {
        vm.emit('desktopSize', {
            width : rect.width,
            height : rect.height
        });
    });
    
    rfb.addListener('unknownRect', function (rect) {
        sys.log('received an unknownRect from rfb');
        vm.emit('error', {
            vmId : opts.id,
            action : 'error',
            message : 'received an unknownRect from rfb'
        });
    });

    function JSONSend(client, msg) {
        client.send(JSON.stringify(msg));
    }
    
    this.attach = function (client) {
        clients[client] = {};
        clients[client]['send'] = function (msg) {
            client.send(JSON.stringify(msg));
        };
        this.addListener('error', function (msg) {
            msg.action = 'error';
            msg.vmId = opts.id;
            JSONSend(client, msg);
        });

        DNode.connect(9200, function (dnode, remote) {
            vm.addListener('png', function (msg) {
                msg.action = 'updateScreen';
                msg.vmId = opts.id;
                msg.imageType = 'png';
                JSONSend(client, msg);

                if (screenShotRequests && msg.fullScreen) {
                    screenShotRequests--;
                    remote.screenshot(msg.image.toString('binary'),
                    function (screenshot) {
                        if (screenshot.status == 'error') {
                            JSONSend(client, {
                                action : 'error',
                                vmId : opts.id,
                                message : screenshot.message
                            });
                            return;
                        }
                        JSONSend(client, {
                            action : 'screenshot',
                            vmId : opts.id,
                            screenshotUrl : 'http://.../' + screenshot.filename
                        });
                    });
                }
            });
        });

        this.addListener('copyRect', function (msg) {
            msg.action = 'copyRect';
            msg.vmId = opts.id;
            JSONSend(client, msg);
        });
        this.addListener('desktopSize', function (msg) {
            msg.action = 'desktopSize';
            msg.vmId = opts.id;
            JSONSend(client, msg);
        });

        clients[client]['send']({
            vmId : opts.id,
            action : 'attached'
        });
    };
    
    this.detach = function (client) {
        this.removeListener('error', clients[client]['send']);
        this.removeListener('png', clients[client]['send']);
        this.removeListener('copyRect', clients[client]['send']);
        this.removeListener('desktopSize', clients[client]['send']);
        delete clients[client];
    };

    this.keyDown = function (key) {
        rfb.sendKeyDown(key);
    };

    this.keyUp = function (key) {
        rfb.sendKeyUp(key);
    };

    this.requestRedrawScreen = function () {
        rfb.requestRedrawScreen();
    };

    this.sendPointer = function (x, y, mask) {
        rfb.sendPointer(x, y, mask);
    };

    this.takeScreenshot = function () {
        rfb.requestRedrawScreen();
        screenShotRequests++;
    };
}

