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
    var screenshotRequests = 0;
    var screencastRequests = 0;
    var screencastId = null;
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
        if (screencastRequests && screencastId) {
            vm.emit('screencastEndPush');
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

            if (screencastRequests && !screencastId && fullScreen) {
                vm.emit('startScreencast', rect.fb, rfbDims.width, rfbDims.height);
            }
        }
        else {
            pngStack.push(rect.fb, rect.x, rect.y, rect.width, rect.height);
        }
        if (screencastRequests && screencastId) {
            vm.emit('screencastPushUpdate', rect.fb, rect.x, rect.y, rect.width, rect.height);
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


        DNode.connect(9200, function (dnode, screenshotRemote) {
            DNode.connect(9300, function (dnode, screencastRemote) {
                vm.addListener('png', function (msg) {
                    msg.action = 'updateScreen';
                    msg.vmId = opts.id;
                    msg.imageType = 'png';
                    JSONSend(client, msg);

                    // todo: move this as an event
                    if (screenshotRequests && msg.fullScreen) {
                        screenshotRequests--;
                        screenshotRemote.screenshot(msg.image.toString('binary'),
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
                                screenshotUrl : 'http://10.1.1.2:9201/' + screenshot.fileName
                            });
                        });
                    }
                });
                vm.addListener('startScreencast', function (fb, width, height) {
                    screencastRemote.startScreencast(width, height, function (videoId) {
                        screencastId = videoId;
                        screencastRemote.newFrame(videoId, fb.toString('binary'),
                            function () {}
                        );
                    });
                });
                vm.addListener('stopScreencast', function () {
                    screencastRemote.stopScreencast(screencastId,
                        function (sc) {
                            JSONSend(client, {
                                action : 'stopScreencast',
                                vmId : opts.id,
                                screencastUrl : 'http://10.1.1.2:9301/' + sc.fileName
                            });
                        }
                    );
                });
                vm.addListener('screencastPushUpdate', function (fb, x, y, w, h) {
                    screencastRemote.pushUpdate(screencastId,
                        fb.toString('binary'), x, y, w, h, function (){}
                    );
                });
                vm.addListener('screencastEndPush', function () {
                    screencastRemote.endPush(screencastId,
                        function (){}
                    );
                });
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
        if (!screenshotRequests) {
            screenshotRequests++;
            rfb.requestRedrawScreen();
        }
    };

    this.startScreencast = function () {
        if (!screencastRequests) {
            screencastRequests++;
            rfb.requestRedrawScreen();
        }
    };

    this.stopScreencast = function () {
        if (screencastRequests) {
            vm.emit('stopScreencast');
        }
    };

}

