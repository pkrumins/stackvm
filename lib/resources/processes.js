// Manage processes

var EventEmitter = require('events').EventEmitter;
var Process = require('./process');
var DNode = require('dnode');
 
module.exports = function Processes (params) {
    var user = params.user;
    var disks = params.disks;
    var manager = params.manager;
    var conn = params.connection;
    
    var self = new EventEmitter;
    self.connections = 0;
    if (user.name in Processes) {
        self = Processes[user.name];
    }
    else {
        DNode.expose(self, 'on');
        self.processes = Object.keys(params.processes)
            .reduce(function (acc,addr) {
                var p = new Process({
                    connection : conn,
                    proc : params.processes[addr],
                    mode : 'rwx' // since we own these processes
                });
                proc.on('kill', function () { self.emit('kill', addr) });
                proc.on('exit', function () { self.emit('exit', addr) });
                
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
                    self.processes[proc.addr] = p;
                    self.emit('spawn', p);
                }
            ) },
        };
    }
    
    self.list = function (cb) { cb(Object.keys(disks).map(Disk)) };
    
    Processes[user.name] = self;
    return self;
};

