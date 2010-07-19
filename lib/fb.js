// Wrap around the encoder and rfb events so that multiple dnode clients can
// manage events without interference.

var EventEmitter = require('events').EventEmitter;
var sys = require('sys');

exports.FB = FB;
function FB (params) {
    var self = this;
    
    // use this EventEmitter instead of FB.prototype so dnode can see it
    var ev = new EventEmitter;
    'addListener removeListener'.split(' ').forEach(function (method) {
        self[method] = function () {
            var args = [].slice.call(arguments);
            ev[method].apply(ev,args);
        };
    });
    
    var encoder = params.encoder;
    var rfb = params.rfb;
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
                encoder.addListener(key, listeners[key]);
            });
            attached = true;
        }
    };
    
    self.detach = function () {
        if (attached == true) {
            events.forEach(function (key) {
                encoder.removeListener(key, listeners[key]);
            });
            attached = false;
        }
    };
    
    // Poll the rfb's dimensions.
    // The callback returns a hash with width and height.
    self.dimensions = function (cb) {
        cb(rfb.fbDims());
    };
    
    self.requestRedrawScreen = rfb.requestRedrawScreen;
    
    self.inputEnabled = 'inputEnabled' in params
        ? params.inputEnabled : true;
    
    'sendKeyDown sendKeyUp sendPointer'.split(/\s+/).forEach(function (method) {
        self[method] = function () {
            var args = [].slice.call(arguments);
            if (self.inputEnabled) {
                rfb[method].apply(rfb,args);
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
    };
    
    var screencastFunc = null;
    self.startScreencast = function (name) {
        screencastFunc = function () {
            // ...
        };
        encoder.addListener('png', screencastFunc);
    };
    
    self.stopScreencast = function (cb) {
        encoder.removeListener('png', screencastFunc);
        cb(baseURIs.screencast + '/' + 'not-implemented');
    };
}

