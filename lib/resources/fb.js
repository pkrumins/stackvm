// Connect to an RFB service, exposing an input to send (keyboard, mouse) to and
// an encoder to grab framebuffer updates from.

var RFB = require('rfb');
var Resource = require('../resource');
var Input = require('./fb/input');
var Encoder = require('./fb/encoder');

module.exports = function FB (params, cb) {
    if (params.addr in FB) {
        // An FB is already connected, just return that one.
        // Slight possibility of race condition here
        // when two clients try to get a new handle at the same time
        cb(FB[params.addr]);
    }
    else {
        var rfb = new RFB({
            host : params.addr.split(/:/)[0],
            port : params.addr.split(/:/)[1],
            engine : params.engine,
        });
        
        var encoder = new Encoder(rfb);
        
        var fb = {
            input : new Input(rfb),
            encoder : encoder,
            resources : {
                encoder : new Resource(encoder),
            },
            size : null,
            end : function () {
                encoder.emit('end');
                rfb.end(); // rfb should probably emit an event itself, but meh
                delete FB[params.addr];
            },
        };
        
        encoder.on('desktopSize', function (size) {
            fb.size = size;
        });
        
        encoder.dimensions(function (size) {
            fb.size = size;
            FB[params.addr] = fb;
            cb(fb);
        });
    }
};

