// Wrap around the encoder and rfb events so that multiple dnode clients can
// manage events without interference.

module.exports = function Input (rfb) {
    this.sendKeyDown = function () {
        rfb.sendKeyDown([].slice.call(arguments));
    };
    
    this.sendKeyUp = function () {
        rfb.sendKeyUp([].slice.call(arguments));
    };
    
    this.sendPointer = function () {
        rfb.sendPointer([].slice.call(arguments));
    };
}

