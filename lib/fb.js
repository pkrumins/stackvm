// Wrap around the encoder and rfb events so that multiple dnode clients can
// manage events without interference.

var EventEmitter = require('events').EventEmitter;
var RFB = require('rfb').RFB;
var Encoder = require('./encoder').Encoder;

exports.FB = FB;
function FB (params) {
    var self = this;
    
    // use this EventEmitter instead of FB.prototype so dnode can see it
    var ev = new EventEmitter;
    self.on = function () {
        var args = [].slice.call(arguments);
        ev.on.apply(ev,args);
    };
    
    var rfb = new RFB({ port : params.port });
    var encoder = new Encoder(rfb);
    
    var baseURIs = params.baseURIs || {};
    
    var listeners = {};
    var events = 'error png copyRect desktopSize'.split(/\s+/);
    events.forEach(function (name) {
        listeners[name] = function () {
            var args = [name].concat([].slice.call(arguments));
            ev.emit.apply(ev,args);
        }
    });
    
    var attached = false;
    
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
console.log('emit close');
            ev.emit('close');
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
        cb(rfb.fbDims());
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
                ev.emit('error', 'input not enabled');
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

