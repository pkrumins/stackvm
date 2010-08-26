// Connect to an RFB service, exposing an input to send (keyboard, mouse) to and
// an encoder to grab framebuffer updates from.

var RFB = require('rfb');
var Input = require('./framebuffer/input');
var Encoder = require('./framebuffer/encoder');
var Model = require('../model');

module.exports = Model(Framebuffer);
function Framebuffer (params, cb) {
    var rfb = new RFB({
        host : params.addr.split(/:/)[0],
        port : params.addr.split(/:/)[1],
        engine : params.engine,
    });
    
    var fb = {
        input : new Input(rfb),
        encoder : new Encoder(rfb),
        size : null,
        end : function () {
            fb.encoder.emit('end');
            rfb.end();
        },
    };
    
    fb.encoder.on('desktopSize', function (size) {
        fb.size = size;
    });
    
    fb.encoder.dimensions(function (size) {
        fb.size = size;
        cb(fb);
    });
};

