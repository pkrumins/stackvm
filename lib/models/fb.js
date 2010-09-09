// Connect to an RFB service, exposing an input to send (keyboard, mouse) to and
// an encoder to grab framebuffer updates from.

var RFB = require('rfb');
var Input = require('./fb/input');
var Encoder = require('./fb/encoder');

module.exports = function FB (addr, engine, cb) {
    var rfb = new RFB({
        host : addr.split(/:/)[0],
        port : addr.split(/:/)[1],
        engine : engine,
    });
    
    var fb = {
        input : new Input(rfb),
        encoder : new Encoder(rfb),
        size : null,
        end : function () {
            fb.encoder.emit('end');
            rfb.end(); // rfb should probably emit an event itself, but meh
        },
    };
    
    fb.encoder.on('desktopSize', function (size) {
        fb.size = size;
    });
    
    rfb.on('error', cb);
    
    fb.encoder.dimensions(function (size) {
        rfb.removeListener('error', cb);
        fb.size = size;
        FB[addr] = fb;
        cb(null, fb);
    });
};

