// Wrap around the rfb module for so that multiple dnode clients can register
// events without stepping on each others toes

var EventEmitter = require('events').EventEmitter;

exports.FB = FB;
FB.prototype = new EventEmitter;
function FB (params) {
    var self = this;
    var rfb = params.rfb;
    
    var listeners = {};
    'error png copyRect desktopSize'.split(/\s+/).forEach(function (ev) {
        listeners[ev] = function () {
            var args = [ev].concat([].slice.call(arguments));
            self.emit.apply(self,args);
        }
    });
    
    self.attach = function () {
        Object.keys(listeners).forEach(function (key) {
            rfb.addListener(key, listeners[key]);
        });
    };
    
    self.detach = function () {
        Object.keys(listeners).forEach(function (key) {
            rfb.removeListener(key, listeners[key]);
        });
    };
    
    'sendKeyDown sendKeyUp requestRedrawScreen sendPointer'
        .split(/\s+/).forEach(function (method) {
            self[method] = rfb[method];
        });
}

