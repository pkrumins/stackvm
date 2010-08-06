var child = require('child_process');
var EventEmitter = require('events').EventEmitter;

module.exports = Qemu;
Qemu.prototype = new EventEmitter;
function Qemu (processes) {
    var self = this;
    if (!(self instanceof Qemu)) return new Qemu(processes);
    if (!self.processes) self.processes = [];
    
    self.isAlive = function (proc, f) {
        try {
            process.kill(proc.pid,0);
            f(true);
        } catch (e) {
            f(false);
        }
    };
    
    self.spawn = function (user, disk, f) {
        var port = 5900;
        for (; 'localhost:' + port in self.processes; port++);
        var addr = 'localhost:' + port; // for now
        
        console.log('firing up qemu on ' + addr);
        
        var qemu = child.spawn('qemu', [
            '-vnc', ':' + (port - 5900),
            __dirname + '/../../users/' + user.name + '/disks/' + disk
        ]);
        
        var proc = new EventEmitter;
        proc.disk = disk;
        proc.engine = 'qemu';
        proc.user = user;
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
    
    self.kill = function (proc, f) {
        if (proc.addr in self.processes) {
            process.kill(self.processes[proc.addr].pid);
            delete self.processes[proc.addr];
            if (f) f(true);
        }
        else {
            if (f) f(false);
        }
        // note: 'exit' will probably fire automatically
    };
}

