var child = require('child_process');
var RemoteEmitter = require('dnode/events');

var ports = [];

module.exports = function Qemu () {
    var self = this;
    if (!(self instanceof Qemu)) return new Qemu();
    
    self.isAlive = function (proc, f) {
        try {
            process.kill(proc.pid, 0);
            f(true);
        } catch (e) {
            f(false);
        }
    };
    
    self.spawn = function (filename, cb) {
        var port = 5900;
        for (; ports.indexOf(port) >= 0; port++);
        var addr = 'localhost:' + port; // for now
        
        console.log('firing up qemu on ' + addr);
        
        var qemu = child.spawn('qemu', [
            '-vnc', ':' + (port - 5900),
            filename.match(/\.iso$/) ? '-cdrom' : '-hda', filename
        ]);
        
        var proc = new RemoteEmitter;
        proc.disk = filename;
        proc.engine = 'qemu';
        proc.addr = addr;
        proc.port = port;
        proc.pid = qemu.pid;
        
        proc.kill = function (cb) {
            try {
                process.kill(qemu.pid);
                if (cb) cb(true);
            }
            catch (err) {
                if (cb) cb(false);
            }
        };
        
        qemu.on('exit', function () {
            proc.kill = function (cb) { if (cb) cb(false) };
            
            var i = ports.indexOf(port);
            if (i >= 0) ports.splice(i, 1);
            
            proc.emit('exit');
            
            console.log('qemu on ' + addr + ' died');
        });
        
        ports.push(port);
        cb(proc);
    };
}

