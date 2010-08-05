// Manage processes

var EventEmitter = require('events').EventEmitter;
var Resource = require('../resource');
var Process = require('./process');
 
module.exports = function Processes (params) {
    var user = params.user;
    var disks = params.disks;
    var manager = params.manager;
    var processes = params.processes;
    
    var self = null;
    if (user.name in Processes) {
        self = Processes[user.name];
    }
    else {
        self = new EventEmitter;
        self.resource = new Resource(self);
        self.connections = 0;
        Object.keys(processes).forEach(function (addr) {
            var proc = processes[addr];
            proc.on('kill', function () {
                self.emit('kill', addr);
            });
        });
    }
    
    function Disk (file, conn) {
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
    
    return {
        bind : function (em, conn) {
            self.connections ++;
            self.resource.subscribe(em, conn);
            em.emit('list', Object.keys(disks).map(function (file) {
                return Disk(file,conn);
            }));
            
            conn.on('end', function () {
                self.connections --;
                if (self.connections <= 0) {
                    delete Processes[user.name];
                }
            });
        },
    };
};

