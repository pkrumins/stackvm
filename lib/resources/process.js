var EventEmitter = require('events').EventEmitter;
var FB = require('./fb');
var Png = require('png').Png;
var fs = require('fs');

module.exports = Process;
Process.prototype = new EventEmitter;
function Process (params) {
    if (!(this instanceof Process)) return new Process(params);
    var self = this;
    
    self.addr = params.proc.addr;
    self.engine = params.proc.engine;
    self.disk = params.proc.disk;
    self.pid = params.proc.pid;
    self.name = params.name;
    
    params.proc.on('exit', function () { self.emit('exit', self.addr) });
    
    self.attach = function (cb) {
        FB({ addr : self.addr, engine : self.engine }, function (fb) {
            cb({
                input : fb.input,
                encoder : fb.encoder,
                size : fb.size,
            });
        });
    };
    
    self.kill = function () {
        if (self.addr in FB) FB[self.addr].end();
        self.emit('kill');
    };

    self.screenshot = function () {
        FB({ addr : self.addr, engine : self.engine }, function (fb) {
            fb.encoder.requestRedraw();
            fb.encoder.once('raw', function g (rect) {
                fb.encoder.dimensions(function (dims) {
                    var fullScreen = rect.x == 0 && rect.y == 0 &&
                        rect.width == dims.width && rect.height == dims.height;
                    if (!fullScreen) {
                        fb.encoder.once('raw', g);
                    }
                    else {
                        var png = new Png(rect.fb, rect.width, rect.height, 'bgr').
                            encode(function (image) {
                                var fileName = randomFileName();
                                var fullPath = __dirname + '/../../static/screenshots/' +
                                    fileName;
                                    
                                fs.writeFile(fullPath, image, 'binary',
                                    function (err) {
                                        if (err) {
                                            console.log('failed writing screenshot: ' + err);
                                        }
                                        else {
                                            self.emit('screenshot',
                                                'http://10.1.1.2:9000/screenshots/' + fileName);
                                        }
                                });
                            });
                    }
                });
            });
        });
    };
}

// this shouldn't be here. todo: refactor it all out to screenshots.js
function randomFileName () {
    var fileName = '';
    for (var i = 0; i<32; i++) {
        fileName += String.fromCharCode(Math.random()*26 + 97);
    }
    return fileName;
}

