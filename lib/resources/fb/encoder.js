/*  Emit png (and soon jpeg) tiles from an RFB stream.
    Emits these events:
        error, screenUpdate, copyRect, desktopSize
*/

var EventEmitter = require('events').EventEmitter;
var Image = require('image');
var Buffer = require('buffer').Buffer;
var base64 = require('base64');

module.exports = Encoder;
Encoder.prototype = new EventEmitter;
function Encoder (rfb) {
    if (!(this instanceof Encoder)) return new Encoder(rfb);
    var self = this;
    var imageStack = null;
    var imageType = 'gif';
    var bufType = 'bgr';
    
    // Execute a callback only the first time it occurs.
    self.once = function (ev, f) {
        self.on(ev, function g () {
            var args = [].slice.call(arguments);
            f.apply(self,args);
            self.removeListener(ev,g);
        });
    };
    
    // Request a new complete framebuffer from the rfb server.
    self.requestRedrawScreen = function () {
        rfb.requestRedrawScreen();
    };
    
    // Poll the rfb's dimensions.
    // The callback returns a hash with width and height.
    self.dimensions = function (cb) {
        rfb.fbDims(function (size) {
            cb(size);
        });
    };
    
    // pass some events through directly to consumers
    'error copyRect desktopSize'.split(/\s+/).forEach(function (ev) {
        rfb.on(ev, function () {
            var args = [ev].concat([].slice.call(arguments));
            self.emit.apply(self,args);
        });
    });
    
    rfb.on('startRects', function (nRects) {
        if (nRects > 1) {
            imageStack = new Image.Stack(imageType, bufType);
        }
    });
    
    rfb.on('endRects', function (nRects) {
        if (nRects > 1) {
            var image = imageStack.encode();
            var imageBuf = new Buffer(image.length);
            imageBuf.write(image, 'binary');
            
            var dims = imageStack.dimensions();
            self.emit('screenUpdate', {
                base64 : base64.encode(imageBuf),
                type : imageType,
                width : dims.width,
                height : dims.height,
                x : dims.x,
                y : dims.y
            });
        }
    });

    rfb.on('raw', function (rect) {
        if (rect.nRects == 1) {
            var image = new Image(imageType, bufType)
                .encode(rect.fb, rect.width,rect.height);
            var imageBuf = new Buffer(image.length);
            imageBuf.write(image, 'binary');
            
            rfb.fbDims(function (dims) {
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
            });
        }
        else {
            imageStack.push(rect.fb, rect.x, rect.y, rect.width, rect.height);
        }
    });
    
    rfb.on('unknownRect', function (rect) {
        var sys = require('sys');
        sys.log('received an unknownRect from rfb: ' + sys.inspect(rect));
        self.emit('error', 'received an unknownRect from rfb');
    });
}

