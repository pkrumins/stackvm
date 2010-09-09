var child = require('child_process');
var path = require('path');
var EventEmitter = require('events').EventEmitter;
var Process = require('../process');
var Hash = require('traverse/hash');

var ports = [];

module.exports = Qemu;
Qemu.prototype = new EventEmitter;
function Qemu () {
    var self = this;
    if (!(self instanceof Qemu)) return new Qemu();
    
    self.processes = {};
    
    self.connect = function (p) {
        try { process.kill(p.pid, 0) }
        catch (err) { return null }
        
        var proc = Process(Hash.merge(p, {
            engine : 'qemu',
            kill : function (cb) {
                try {
                    process.kill(proc.pid);
                    if (cb) cb(true);
                }
                catch (err) {
                    if (cb) cb(false);
                }
            }
        }));
        
        self.processes[proc.address] = proc;
        self.emit('connect', proc);
        return proc;
    };
    
    self.spawn = function (filename, cb) {
        var port = 5900;
        for (; ports.indexOf(port) >= 0; port++);
        var addr = 'localhost:' + port; // for now
        ports.push(port);
        
        console.log('firing up qemu on ' + addr);
        
        var disowner = __dirname + '/../../../bin/disowner.pl';
        var qemu = child.spawn(disowner, [
            'qemu', '-vnc', ':' + (port - 5900),
            filename.match(/\.iso$/) ? '-cdrom' : '-hda', filename
        ]);
        qemu.stdout.on('data', function (buf) {
            var proc = self.connect({
                filename : path.basename(filename),
                address : addr,
                pid : parseInt(buf.toString(), 10),
            });
            self.emit('spawn', proc);
            
            proc.on('exit', function () {
                console.log('qemu on ' + addr + ' died');
                var i = ports.indexOf(port);
                if (i >= 0) ports.splice(i, 1);
                delete self.processes[addr];
            });
            
            proc.on('ready', function () { cb(proc) });
        });
    };
}

