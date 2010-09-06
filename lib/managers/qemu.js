var child = require('child_process');
var EventEmitter = require('events').EventEmitter;

module.exports = Qemu;
Qemu.prototype = new EventEmitter;
function Qemu () {
    var self = this;
    if (!(self instanceof Qemu)) return new Qemu();
    if (!self.processes) self.processes = [];
    
    self.isAlive = function (proc, f) {
        try {
            process.kill(proc.pid,0);
            f(true);
        } catch (e) {
            f(false);
        }
    };
    
    self.spawn = function (params, f) {
        var port = 5900;
        for (; 'localhost:' + port in self.processes; port++);
        var addr = 'localhost:' + port; // for now
        
        console.log('firing up qemu on ' + addr);
        
        var file = __dirname + '/../../users/'
            + params.user.name + '/disks/' + params.file;
        var qemu = child.spawn('qemu', [
            '-vnc', ':' + (port - 5900),
            file.match(/\.iso$/) ? '-cdrom' : '-hda', file
        ]);
        
        var proc = new EventEmitter;
        proc.disk = file;
        proc.engine = 'qemu';
        proc.user = params.user;
        proc.addr = addr;
        proc.pid = qemu.pid;
        
        qemu.on('exit', function () {
            delete self.processes[addr];
            proc.emit('exit');
            self.emit('exit', proc);
            console.log('qemu on ' + addr + ' died');
        });
        
        self.processes[addr] = proc;
        if (f) f(proc);
    };
    
    self.kill = function (addr, f) {
        if (addr in self.processes) {
            var proc = self.processes[addr];
            process.kill(proc.pid);
            delete self.processes[addr];
            if (f) f(true);
        }
        else {
            if (f) f(false);
        }
        // note: 'exit' will probably fire automatically
    };
}

