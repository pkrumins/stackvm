var EventEmitter = require('events').EventEmitter;
var FB = require('./fb');

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
}

