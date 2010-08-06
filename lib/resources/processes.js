// Manage processes

var EventEmitter = require('events').EventEmitter;
var Process = require('./process');
 
module.exports = function Processes (params) {
    var user = params.user;
    var disks = params.disks;
    var manager = params.manager;
    var processes = params.processes;
    var conn = params.connection;
    
    var self = new EventEmitter;
    self.connections = 0;
    if (user.name in Processes) {
        self = Processes[user.name]
    }
    else {
        Object.keys(processes).forEach(function (addr) {
            var proc = processes[addr];
            proc.on('kill', function () {
                self.emit('kill', addr);
            });
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
                    proc.on('kill', function () {
                        self.emit('kill', proc.addr);
                    });
                    processes[proc.addr] = p;
                    self.emit('spawn', p);
                }
            ) },
        };
    }
    
    self.list = function (cb) { cb(Object.keys(disks).map(Disk)) };
    
    return self;
};

