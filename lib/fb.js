// Wrap around the encoder and rfb events so that multiple dnode clients can
// manage events without interference.

var EventEmitter = require('events').EventEmitter;

exports.FB = FB;
FB.prototype = new EventEmitter;
function FB (params) {
    var self = this;
    var encoder = params.encoder;
    var rfb = params.rfb;
    
    var listeners = {};
    var events = 'error png copyRect desktopSize'.split(/\s+/);
    evs.forEach(function (ev) {
        listeners[ev] = function () {
            var args = [ev].concat([].slice.call(arguments));
            self.emit.apply(self,args);
        }
    });
    
    self.attach = function () {
        events.forEach(function (key) {
            encoder.addListener(key, listeners[key]);
        });
    };
    
    self.detach = function () {
        events.forEach(function (key) {
            encoder.removeListener(key, listeners[key]);
        });
    };
    
    self.inputEnabled = false;
    
    'sendKeyDown sendKeyUp requestRedrawScreen sendPointer'
        .split(/\s+/).forEach(function (method) {
            self[method] = function () {
                var args = [].slice.call(arguments);
                if (self.inputEnabled) {
                    rfb[method].apply(rfb,args);
                }
                else {
                    self.emit('error', 'input not enabled');
                }
            };
        });
}

