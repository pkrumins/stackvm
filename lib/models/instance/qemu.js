var child = require('child_process');
var RemoteEmitter = require('../../remote');

module.exports = Qemu;

Qemu.spawn = function (params, cb) {
    var filename = params.disk.filename;
    console.log('firing up qemu on port ' + port);
    
    params.proc = child.spawn('qemu', [
        '-vnc', ':' + (port - 5900),
        filename.match(/\.iso$/) ? '-cdrom' : '-hda',
        filename
    ]);
    
    // A little time for the vnc server to fire up:
    setTimeout(function () {
        cb(new Qemu(params));
    }, 500);
};

Qemu.prototype = new RemoteEmitter;
function Qemu (params) {
    var self = this;
    
    self.addr = params.host + ':' + params.port;
    self.port = params.port;
    self.engine = 'qemu';
    
    self.alive = function (cb) {
        try {
            process.kill(proc.pid, 0);
            cb(true);
        } catch (e) {
            cb(false);
        }
    };
    
    proc.on('exit', function () {
        console.log('qemu on port ' + port + ' died');
        self.emit('exit');
        self.alive = function (cb) { cb(false) };
    });
    
    self.kill = function (cb) {
        self.alive(function (isAlive) {
            if (isAlive) {
                try {
                    process.kill(proc.pid);
                    if (cb) cb(true);
                }
                catch (e) {
                    if (cb) cb(false);
                }
            }
            else { cb(false) }
        });
    };
}

