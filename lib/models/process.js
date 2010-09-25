var fs = require('fs');
var path = require('path');

var RemoteEmitter = require('dnode/events');
var Hash = require('traverse/hash');
var Png = require('png').Png;
var base64 = require('base64');

var Access = require('./access');
var FB = require('./fb');

module.exports = Process;
Process.prototype = new RemoteEmitter;
function Process (params) {
    if (!(this instanceof Process)) return new Process(params);
    var self = this;
    self.__proto__ = Hash.copy(self.__proto__);
    
    self.engine = p.engine;
    
    Hash.update(self, Hash.extract(
        params, 'engine kill filename address pid'.split(' ')
    ));
    
    var thumbDir = params.userdir + '/thumbs/' + self.filename;
    path.exists(thumbDir, function (exists) {
        if (!exists) fs.mkdir(thumbDir, 0755);
    });
    
    function saveRect (filename, rect) {
        fs.createWriteStream(filename)
            .write(rect.base64, 'base64');
    }
    
    var framebuffer = null;
    
    self.on('attach', function (tied) {
        tied.fb = {
            input : framebuffer.input,
            encoder : tied.tie(framebuffer.encoder),
            size : framebuffer.size,
        };
    });
    
    setTimeout(function () { // lame hack, give it time to connect
        FB(self.address, self.engine, function (err, fb) {
            if (err) throw err;
            
            fb.encoder.on('end', function () {
                self.emit('exit');
            });
            framebuffer = fb;
            self.emit('ready');
            
            var elapsed = 0;
            var lastUpdate = 0;
            function update () {
                if (self.connections == 0) return;
                if ((Date.now() - lastUpdate) / 1000 > 10) {
                    fb.encoder.requestRedraw();
                }
                lastUpdate = Date.now();
            }
            
            fb.encoder.on('raw', update);
            fb.encoder.on('copyRect', update);
            
            fb.encoder.on('screenUpdate', function (rect) {
                if (rect.fullScreen && self.connections > 0) {
                    var file = thumbDir + '/' + self.address + '.png';
                    saveRect(file, rect);
console.log('emit thumb');
                    self.emit('thumb', self.address + '.png');
                }
            });
        });
    }, 500);
    
    self.__proto__.access = Access(params.rules || {});
    
    self.__proto__.limit = function (user, access) {
        var can = (access ? access : self.access).allowed(user);
        
        var share = Hash.copy(this);
        share.fb = Hash.copy(share.fb);
        
        if (!can.input) delete share.fb.input;
        if (!can.view) {
            delete share.fb.encoder;
            delete share.subscribe;
        }
        if (!can.kill) delete share.kill;
        
        return share;
    };
    
    // probably this will need tied to notify the client
    self.screenshot = function () {
        FB(self.address, self.engine, function (err, fb) {
            fb.encoder.requestRedrawScreen();
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

