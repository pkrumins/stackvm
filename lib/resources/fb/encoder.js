/*  Emit png (and soon jpeg) tiles from an RFB stream.
    Emits these events:
        error, screenUpdate, copyRect, desktopSize
*/

var sys = require('sys');
var EventEmitter = require('events').EventEmitter;
var ImageLib = require('image');
var Buffer = require('buffer').Buffer;
var base64 = require('base64');
var DNode = require('dnode');

module.exports = Encoder;
Encoder.prototype = new EventEmitter;
function Encoder (rfb) {
    if (!(this instanceof Encoder)) return new Encoder(rfb);
    var self = this;
    var imageStack = null;
    var imageType = 'png';
    var bufType = 'bgr';
    
    DNode.expose(self, 'on');
    DNode.expose(self, 'removeListener');
    
    // Execute a callback only the first time it occurs.
    self.once = function (ev, f) {
        self.on(ev, function g () {
            var args = [].slice.call(arguments);
            f.apply(self,args);
            self.removeListener(ev,g);
        });
    };
    
    // Request a new complete framebuffer from the rfb server.
    self.requestRedraw = function () {
        rfb.requestRedraw();
    };
    
    // Poll the rfb's dimensions.
    // The callback returns a hash with width and height.
    self.dimensions = rfb.dimensions;
    
    // pass some events through directly to consumers
    'end error'.split(/\s+/).forEach(function (ev) {
        rfb.on(ev, function () {
            var args = [ev].concat([].slice.call(arguments));
            self.emit.apply(self,args);
        });
    });
    
    rfb.on('desktopSize', function (rect) {
        self.emit('desktopSize', { width : rect.width, height : rect.height });
    });

    rfb.on('copyRect', function (rect) {
        self.emit('copyRect', {
            srcX : rect.srcX,
            srcY : rect.srcY,
            width : rect.width,
            height : rect.height,
            x : rect.x,
            y : rect.y
        });
    });
    
    rfb.on('startRects', function (nRects) {
        if (nRects > 1) {
            imageStack = new ImageLib.Stack(imageType, bufType);
        }
    });
    
    rfb.on('endRects', function (nRects) {
        if (nRects > 1) {
            imageStack.encode(function (image, dims) {
                var imageBuf = new Buffer(image.length);
                imageBuf.write(image, 'binary');
                self.emit('screenUpdate', {
                    base64 : base64.encode(imageBuf),
                    type : imageType,
                    width : dims.width,
                    height : dims.height,
                    x : dims.x,
                    y : dims.y
                });
            });
        }
    });

    rfb.on('raw', function (rect) {
        self.emit('raw', rect);
        if (rect.nRects == 1) {
            rfb.dimensions(function (dims) {
                var image = new ImageLib.Image(imageType, bufType).encode(
                    rect.fb, rect.width, rect.height, function (image)
                    {
                        var imageBuf = new Buffer(image.length);
                        imageBuf.write(image, 'binary');
                        var fullScreen = (rect.width == dims.width) &&
                            (rect.height == dims.height);
                        self.emit('screenUpdate', {
                            base64 : base64.encode(imageBuf),
                            type : imageType,
                            width : rect.width,
                            height : rect.height,
                            x : rect.x,
                            y : rect.y,
                            fullScreen : fullScreen
                        });
                    }
                );
            });
        }
        else {
            imageStack.push(rect.fb, rect.x, rect.y, rect.width, rect.height);
        }
    });
    
    rfb.on('unknownRect', function (rect) {
        console.log('received an unknownRect from rfb: ' + sys.inspect(rect));
        self.emit('error', 'received an unknownRect from rfb');
    });
}

