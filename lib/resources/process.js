var EventEmitter = require('events').EventEmitter;
var FB = require('./fb');

module.exports = Process;
Process.prototype = new EventEmitter;
function Process (params) {
    if (!(this instanceof Process)) return new Process(params);
    var self = this;
    
    var conn = params.connection;
    self.addr = params.proc.addr;
    self.engine = params.proc.engine;
    self.disk = params.proc.disk;
    self.pid = params.proc.pid;
    
    params.proc.on('exit', function () { self.emit('exit', self.addr) });
    
    // params.mode is a string containing some arrangement of "rwx"
    var mode = 'r w x'.split(' ').reduce(function (acc,x) {
        acc[x] = params.mode.indexOf(x) >= 0; return acc;
    }, {});
    
    self.attach = function (cb) {
        if (!mode.r) { cb(null); return }
        FB({ addr : self.addr, engine : self.engine }, function (fb) {
            cb({
                input : mode.w ? fb.input : null,
                encoder : fb.encoder,
                size : fb.size,
            });
        });
    };
    
    self.kill = function () {
        if (!mode.x) return;
        if (addr in FB) FB[addr].end();
        self.emit('kill');
    };
}

