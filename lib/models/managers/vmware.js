var EventEmitter = require('events').EventEmitter;
var net = require('net');

module.exports = VMWare;
VMWare.prototype = new EventEmitter;
function VMWare () {
    var self = this;
    if (!(self instanceof VMWare)) return new VMWare();
    if (!self.processes) self.processes = [];
    
    self.isAlive = function (proc, f) {
        var host = proc.addr.split(':')[0];
        var port = proc.addr.split(':')[1];
        
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
     
    self.spawn = function (params, f) {
        var proc = new EventEmitter;
        proc.disk = params.file;
        proc.engine = 'vmware';
        proc.user = params.user;
        proc.addr = params.disk.host;
        proc.pid = 0;

        self.processes[params.disk.host] = proc;
        if (f) f(proc);
    };
    
    self.kill = function (proc, f) {
        delete self.processes[proc.addr];
        if (f) f(true);
    };
}

