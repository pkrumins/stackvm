// Wrap around the encoder and rfb events so that multiple dnode clients can
// manage events without interference.

module.exports = function Input (rfb) {
    this.sendKeyDown = function () {
        rfb.sendKeyDown.apply(rfb, [].slice.call(arguments));
    };
    
    this.sendKeyUp = function () {
        rfb.sendKeyUp.apply(rfb, [].slice.call(arguments));
    };
    
    this.sendPointer = function () {
        rfb.sendPointer.apply(rfb, [].slice.call(arguments));
    };
}

