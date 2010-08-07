// Manage processes

var EventEmitter = require('events').EventEmitter;
var RemoteEmitter = require('../remote_emitter');
var Process = require('./process');
var DNode = require('dnode');
 
module.exports = function Processes (params) {
    var user = params.user;
    var disks = params.disks;
    var manager = params.manager;
    var conn = params.connection;
    
    var self = new EventEmitter;
    if (user.name in Processes) {
        self = Processes[user.name];
    }
    else {
        self.connections = 0;
        self.processes = Object.keys(params.processes)
            .reduce(function (acc,addr) {
                var p = new Process({
                    connection : conn,
                    proc : params.processes[addr],
                    mode : 'rwx' // since we own these processes
                });
                p.on('kill', function () {
                    manager.kill(addr);
                    self.emit('kill', addr)
                    delete self.processes[addr];
                });
                p.on('exit', function () {
                    self.emit('exit', addr);
                    delete self.processes[addr];
                });
                
                acc[addr] = p;
                return acc;
            }, {})
        ;
    }
    self.connections ++;
    
    conn.on('end', function () {
        self.connections --;
        if (self.connections <= 0) {
            delete Processes[user.name];
        }
    });
    
    
    function Disk (file) {
        return {
            name : disks[file].name,
            filename : file,
            processes : Object.keys(self.processes).reduce(function (acc,addr) {
                var proc = self.processes[addr];
                if (proc.disk == file) {
                    acc[addr] = proc;
                }
                return RemoteEmitter(acc, conn);
            }, {}),
            spawn : function (engine) { manager.spawn(
                { user : user, disk : file, engine : engine },
                function (proc) {
                    var p = new Process({
                        connection : conn,
                        proc : proc,
                        mode : 'rwx'
                    });
                    p.on('kill', function () {
                        manager.kill(proc.addr);
                        self.emit('kill', proc.addr)
                        delete self.processes[proc.addr];
                    });
                    p.on('exit', function () {
                        self.emit('exit', proc.addr);
                        delete self.processes[proc.addr];
                    });
                    self.processes[proc.addr] = p;
                    self.emit('spawn', p);
                }
            ) },
        };
    }
    
    self.list = function (cb) { cb(Object.keys(disks).map(Disk)) };
    
    Processes[user.name] = self;
    return RemoteEmitter(self, conn);
};

