// Wrap around the encoder and rfb events so that multiple dnode clients can
// manage events without interference.

var EventEmitter = require('events').EventEmitter;

module.exports = function Input (rfb) {
    this.sendKeyDown = function () {
        rfb.sendKeyDown([].slice.call(arguments));
    };
    
    this.sendKeyUp = function () {
        rfb.sendKeyUp([].slice.call(arguments));
    };
    
    this.sendKeyPointer = function () {
        rfb.sendKeyPointer([].slice.call(arguments));
    };
}

