var child = require('child_process');
var path = require('path');
var RemoteEmitter = require('dnode/events');
var Process = require('../process');

var ports = [];

module.exports = function Qemu () {
    var self = this;
    if (!(self instanceof Qemu)) return new Qemu();
    
    self.spawn = function (filename, cb) {
        var port = 5900;
        for (; ports.indexOf(port) >= 0; port++);
        var addr = 'localhost:' + port; // for now
        
        console.log('firing up qemu on ' + addr);
        
        var qemu = child.spawn('qemu', [
            '-vnc', ':' + (port - 5900),
            filename.match(/\.iso$/) ? '-cdrom' : '-hda', filename
        ]);
        ports.push(port);
        
        var alive = true;
        var proc = Process({
            filename : path.basename(filename),
            engine : 'qemu',
            addr : addr,
            mode : 'rwx',
            kill : function (f) {
                if (!alive) { if (f) f(false); return }
                
                try {
                    process.kill(qemu.pid);
                    if (f) f(true);
                }
                catch (err) {
                    if (f) f(false);
                }
            }
        });
        
        qemu.on('exit', function () {
            alive = false;
            proc.emit('exit');
            console.log('qemu on ' + addr + ' died');
            
            var i = ports.indexOf(port);
            if (i >= 0) ports.splice(i, 1);
        });
        
        proc.on('ready', function () {
            cb(proc);
        });
    };
}

