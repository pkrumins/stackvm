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
    
    self.connect = function (p) {
        var proc = Process(Hash.merge(p, {
            engine : 'qemu',
            kill : function (cb) {
                if (!proc.alive) {
                    if (cb) cb(false);
                    return;
                }
                
                try {
                    process.kill(proc.pid);
                    if (cb) cb(true);
                }
                catch (err) {
                    if (cb) cb(false);
                }
            }
        }));
        
        self.emit('connect', proc);
        return proc;
    };
    
    self.spawn = function (filename, cb) {
        var port = 5900;
        for (; ports.indexOf(port) >= 0; port++);
        var addr = 'localhost:' + port; // for now
        ports.push(port);
        
        console.log('firing up qemu on ' + addr);
        
        var qemu = child.spawn('qemu', [
            '-vnc', ':' + (port - 5900),
            filename.match(/\.iso$/) ? '-cdrom' : '-hda', filename
        ]);
        
        var proc = self.connect({
            filename : path.basename(filename),
            address : addr,
            pid : qemu.pid,
            alive : true,
        });
        
        self.emit('spawn', proc);
        
        qemu.on('exit', function () {
            proc.alive = false;
            proc.emit('exit');
            console.log('qemu on ' + addr + ' died');
            
            var i = ports.indexOf(port);
            if (i >= 0) ports.splice(i, 1);
            
            self.emit('exit', proc);
        });
        
        proc.on('ready', function () {
            cb(proc && proc.alive ? proc : null);
        });
    };
}

