/*  Emit png (and soon jpeg) tiles from an RFB stream.
    Emits these events:
        error, screenUpdate, copyRect, desktopSize
*/

var sys = require('sys');
var RemoteEmitter = require('dnode/events');
var PngLib = require('png');
var Buffer = require('buffer').Buffer;
var base64 = require('base64');

module.exports = Encoder;
Encoder.prototype = new RemoteEmitter;
function Encoder (rfb) {
    if (!(this instanceof Encoder)) return new Encoder(rfb);
    var self = this;
    var imageStack = null;
    var imageType = 'png';
    var bufType = 'bgr';
    
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
            imageStack = new PngLib.DynamicPngStack(bufType);
        }
    });
    
    rfb.on('endRects', function (nRects) {
        if (nRects > 1) {
            var image = imageStack.encodeSync();
            var dims = imageStack.dimensions();
                
            self.emit('screenUpdate', {
                base64 : base64.encode(image),
                type : imageType,
                width : dims.width,
                height : dims.height,
                x : dims.x,
                y : dims.y
            });
        }
    });

    rfb.on('raw', function (rect) {
        self.emit('raw', rect);
        if (rect.nRects == 1) {
            rfb.dimensions(function (dims) {
                var image = new PngLib.Png(rect.fb, rect.width, rect.height, bufType).encodeSync();
                var fullScreen = (rect.width == dims.width) &&
                    (rect.height == dims.height);
                self.emit('screenUpdate', {
                    base64 : base64.encode(image),
                    type : imageType,
                    width : rect.width,
                    height : rect.height,
                    x : rect.x,
                    y : rect.y,
                    fullScreen : fullScreen
                });
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

