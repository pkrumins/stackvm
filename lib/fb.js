// Wrap around the encoder and rfb events so that multiple dnode clients can
// manage events without interference.

var EventEmitter = require('events').EventEmitter;
var RFB = require('rfb').RFB;
var Encoder = require('./encoder').Encoder;

exports.FB = FB;
FB.prototype = new EventEmitter;
function FB (params) {
    var self = this;
    
    // expose .on so dnode can see it
    self.on = self.on;
    
    var hostPort = params.host.split(':');
    
    var rfb = new RFB({
        host : hostPort[0],
        port : hostPort[1],
        engine : params.engine || 'qemu'
    });
    var encoder = new Encoder(rfb);
    
    var baseURIs = params.baseURIs || {};
    
    var listeners = {};
    var events = 'error png copyRect desktopSize'.split(/\s+/);
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
    
    // render a screenshot and return the uri to the callback
    self.screenshot = function (cb) {
        encoder.onceListener('png', function () {
            // ...
            cb(baseURIs.screenshot + '/' + 'not-implemented');
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

