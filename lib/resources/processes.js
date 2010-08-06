// Manage processes

var EventEmitter = require('events').EventEmitter;
var Process = require('./process');
var DNode = require('dnode');
 
module.exports = function Processes (params) {
    var user = params.user;
    var disks = params.disks;
    var manager = params.manager;
    var conn = params.connection;
    var processes = Object.keys(params.processes)
        .reduce(function (acc,addr) {
            var proc = params.processes[addr];
            acc[addr] = new Process({
                connection : conn,
                proc : proc,
                mode : 'rwx' // since we own these processes
            });
            return acc;
        }, {})
    ;
    
    var self = new EventEmitter;
    self.connections = 0;
    if (user.name in Processes) {
        self = Processes[user.name];
    }
    else {
        DNode.expose(self, 'on');
        Object.keys(processes).forEach(function (addr) {
            var proc = processes[addr];
            proc.on('kill', function () { self.emit('kill', addr) });
            proc.on('exit', function () { self.emit('exit', addr) });
        });
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
            processes : Object.keys(processes).reduce(function (acc,addr) {
                var proc = processes[addr];
                if (proc.disk == file) acc[addr] = proc;
                return acc;
            }, {}),
            spawn : function (engine) { manager.spawn(
                { user : user, disk : file, engine : engine },
                function (proc) {
                    var p = new Process({
                        connection : conn,
                        proc : proc,
                        mode : 'rwx'
                    });
                    p.on('kill', function () { self.emit('kill', proc.addr) });
                    p.on('exit', function () { self.emit('exit', proc.addr) });
                    processes[proc.addr] = p;
                    self.emit('spawn', p);
                }
            ) },
        };
    }
    
    self.list = function (cb) { cb(Object.keys(disks).map(Disk)) };
    
    return self;
};

