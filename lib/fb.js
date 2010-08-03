// Wrap around the encoder and rfb events so that multiple dnode clients can
// manage events without interference.

var EventEmitter = require('events').EventEmitter;
var RFB = require('rfb').RFB;
var Encoder = require('./encoder').Encoder;
var Image = require('image');
var config = require('lib/config');
var fs = require('fs');

exports.FB = FB;
FB.prototype = new EventEmitter;
function FB (params) {
    var self = this;
    
    var hostPort = params.host.split(':');
    
    var rfb = new RFB({
        host : hostPort[0],
        port : hostPort[1],
        engine : params.engine || 'qemu'
    });
    var encoder = new Encoder(rfb);
    
    var listeners = {};
    var events = 'error screenUpdate copyRect desktopSize'.split(/\s+/);
    events.forEach(function (name) {
        listeners[name] = function () {
            var args = [name].concat([].slice.call(arguments));
            self.emit.apply(self,args);
        }
    });

    var attached = false;
    rfb.on('end', function () {
        console.log('Remote RFB stream hung up at ' + params.host);
        self.detach();
    });
    
    self.attach = function () {
        if (attached == false) {
            events.forEach(function (key) {
                encoder.on(key, listeners[key]);
            });
            attached = true;
        }
        return self;
    };
    
    self.detach = function () {
        if (attached == true) {
            events.forEach(function (key) {
                encoder.removeListener(key, listeners[key]);
            });
            self.emit('close');
            attached = false;
        }
        return self;
    };
    
    self.end = function () {
        rfb.end();
        return self;
    };
    
    // Poll the rfb's dimensions.
    // The callback returns a hash with width and height.
    self.dimensions = function (cb) {
        rfb.fbDims(function (size) {
            cb(size);
        });
        return self;
    };
    
    // Request a new complete framebuffer from the rfb server.
    // The rfb server closes the connection if this is called too early.
    self.requestRedrawScreen = function () {
        rfb.requestRedrawScreen();
        return self;
    };
    
    self.inputEnabled = 'inputEnabled' in params
        ? params.inputEnabled : true;
    
    'sendKeyDown sendKeyUp sendPointer'.split(/\s+/).forEach(function (method) {
        self[method] = function () {
            var args = [].slice.call(arguments);
            if (self.inputEnabled) {
                if (attached) rfb[method].apply(rfb,args);
            }
            else {
                self.emit('error', 'input not enabled');
            }
        };
    });
    
    function randomFileName () {
        var fileName = '';
        for (var i = 0; i<32; i++) {
            fileName += String.fromCharCode(Math.random()*26 + 97);
        }
        return fileName;
    }

    // render a screenshot and return the uri to the callback
    self.screenshot = function (cb) {
        encoder.onceListener('screenUpdate', function g (rect) {
            console.log('rect full: ' + rect.fullScreen);
            if (!rect.fullScreen) {
                encoder.onceListener('screenUpdate', g);
            }
            else {
                /*
                 * this when I hack in .rawBuf on all node-{png,gif,jpeg}
                 *
                var image = Image('png', 'bgr').encode(rect.raw, rect.width, rect.height);
                */
                var fileName = randomFileName() + '.' + rect.type;
                var fullPath = __dirname + '/../' + config.screenshots.path + '/' + fileName;
                var image = rect.image;
                fs.writeFile(fullPath, image, 'binary', function (err) {
                    if (err) {
                        console.log('failed writing screenshot: ' + err);
                    }
                    else {
                        console.log('screenshot: ' + fileName);
                        cb(config.screenshots.baseURI + '/screenshots/' + fileName);
                    }
                })
            }
        });
        rfb.requestRedrawScreen();
        return self;
    };
    
    var screencastFunc = null;
    self.startScreencast = function (name) {
        screencastFunc = function () {
            // ...
        };
        encoder.on('png', screencastFunc);
        return self;
    };
    
    self.stopScreencast = function (cb) {
        encoder.removeListener('png', screencastFunc);
        cb(baseURIs.screencast + '/' + 'not-implemented');
        return self;
    };
}

