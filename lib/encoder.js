/*  Emit png (and soon jpeg) tiles from an RFB stream.
    Emits these events:
        error, png, copyRect, desktopSize
*/

var EventEmitter = require('events').EventEmitter;
var PngLib = require('png');
var JpegLib = require('jpeg');
var GifLib = require('gif');
var Buffer = require('buffer').Buffer;
var base64 = require('base64');

function PngStack (buf_type) {
    var pngStack = new PngLib.DynamicPngStack(buf_type);

    this.encode = function () {
        return pngStack.encode();
    };

    this.dimensions = function () {
        return pngStack.dimensions();
    };

    this.push = function (fb, x, y, w, h) {
        pngStack.push(fb,x,y,w,h);   
    };

    this.type = 'png';
}

function GifStack (buf_type) {
    var gifStack = new GifLib.DynamicGifStack(buf_type);

    this.encode = function () {
        return gifStack.encode();
    };

    this.dimensions = function () {
        return gifStack.dimensions();
    };

    this.push = function (fb, x, y, w, h) {
        gifStack.push(fb,x,y,w,h);   
    };

    this.type = 'gif';
}

function JpegStack (buf_type) {
    var jpegStack = new JpegLib.DynamicJpegStack(60, buf_type); // 60 = quality

    this.encode = function () {
        return jpegStack.encode();
    };

    this.dimensions = function () {
        return jpegStack.dimensions();
    };

    this.push = function (fb, x, y, w, h) {
        jpegStack.push(fb,x,y,w,h);   
    };

    this.type = 'jpeg';
}

function PngEncoder (fb, w, h, buf_type) {
    var pngEncoder = new PngLib.Png(fb, w, h, buf_type);

    this.encode = function () {
        return pngEncoder.encode();
    }

    this.type = 'png';
};

function GifEncoder (fb, w, h, buf_type) {
    var gifEncoder = new GifLib.Gif(fb, w, h, buf_type);

    this.encode = function () {
        return gifEncoder.encode();
    }

    this.type = 'gif';
};

function JpegEncoder (fb, w, h, buf_type) {
    var jpegEncoder = new JpegLib.Jpeg(fb, w, h, 60, buf_type); // 60 = quality

    this.encode = function () {
        return jpegEncoder.encode();
    }

    this.type = 'jpeg';
};

var encoders = {
    'stack' : {
        'png' : PngStack,
        'gif' : GifStack,
        'jpeg' : JpegStack
    },
    'once' : {
        'png' : PngEncoder,
        'gif' : GifEncoder,
        'jpeg' : JpegEncoder
    }
};

exports.Encoder = Encoder;
Encoder.prototype = new EventEmitter;
function Encoder (rfb) {
    if (!(this instanceof Encoder)) return new Encoder(rfb);
    var self = this;
    var imageStack = null;
    var imageType = 'gif';
    var bufType = 'bgr';
    
    // Execute a callback only the first time it occurs.
    self.onceListener = function (ev, f) {
        self.on(ev, function g () {
            var args = [].slice.call(arguments);
            f.apply(self,args);
            self.removeListener(ev,g);
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
            if (imageType != 'jpeg')
                imageStack = new encoders['stack'][imageType](bufType);
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
            var image = new encoders['once'][imageType](rect.fb, rect.width, rect.height, 'bgr').encode();
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

            // take special care of jpeg stack
            if (!imageStack && imageType == 'jpeg') {
                var jpegStack = new JpegLib.DynamicJpegStack(60, bufType);
                jpegStack.setBackground(rect.fb, rect.width, rect.height);
                imageStack = jpegStack;
            }
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

