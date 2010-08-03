var EventEmitter = require('events').EventEmitter;
var net = require('net');

module.exports = VMWare;
VMWare.prototype = new EventEmitter;
function VMWare (processes) {
    var self = this;
    if (!(self instanceof VMWare)) return new VMWare(processes);
    if (!self.processes) self.processes = [];
    
    self.isAlive = function (addr, f) {
        var host = addr.split(':')[0];
        var port = addr.split(':')[1];
        
        var stream = net.createConnection(port, host);
        stream.setTimeout(1000);
        stream.on('connect', function () {
            stream.end();
            f(true);
        });
        stream.on('error', function () {
            f(false);
        });
    };
     
    self.spawn = function (user, vm, f) {
        var proc = {
            vm : vm,
            engine : 'vmware',
            host : vm.host,
            pid : 0
        };
        self.processes[vm.addr] = proc;
        if (f) f(proc);
    };
    
    self.kill = function (proc, f) {
        delete self.processes[proc.addr];
    };
}

