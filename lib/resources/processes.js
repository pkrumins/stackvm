// Manage processes

var EventEmitter = require('events').EventEmitter;
var RemoteEmitter = require('dnode/events');
var Process = require('./process');
var DNode = require('dnode');
var managers = require('../managers.js');
 
module.exports = Processes;
Processes.prototype = new RemoteEmitter;
function Processes (params) {
    if (!(this instanceof Processes)) return new Processes(params);
    var self = this;
    
    var user = params.user;
    var disks = params.disks;
    var conn = params.connection;
    
    self.processes = Object.keys(params.processes)
        .reduce(function (acc,addr) {
            var proc = params.processes[addr];
            
            var p = new Process({
                connection : conn,
                proc : proc,
                name : disks[proc.disk].name,
            });
            
            p.on('kill', function () {
                managers[proc.engine].kill(addr);
                
                self.emit('kill', addr)
                delete self.processes[addr];
            });
            
            p.on('exit', function () {
                self.emit('exit', addr);
                delete self.processes[addr];
            });

            p.on('screenshot', function (url) {
                self.emit('screenshot', url);
            });
            
            acc[addr] = p;
            return acc;
        }, {})
    ;
    
    self.share = function (args) {
        var proc = self.processes[args.addr];
        proc.attach(function (fb) {
            args.to.emit('share', {
                name : proc.name,
                from : args.from,
                mode : args.mode,
                fb : {
                    encoder : args.mode.indexOf('r') >= 0
                        ? fb.encoder : null,
                    input : args.mode.indexOf('w') >= 0
                        ? fb.input : null,
                    size : fb.size,
                }
            });
        });
    };
    
    function Disk (file) {
        return {
            name : disks[file].name,
            filename : file,
            host : disks[file].host,
            processes : Object.keys(self.processes).reduce(function (acc,addr) {
                var proc = self.processes[addr];
                if (proc.disk == file) {
                    acc[addr] = RemoteEmitter(proc, conn);
                }
                return acc;
            }, {}),
            spawn : function (engine) {
                var manager = managers[engine];
                manager.spawn({ user : user, file : file }, function (proc) {
                    var p = new Process({
                        connection : conn,
                        proc : proc,
                        name : disks[file].name,
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
                });
            },
        };
    }
    
    self.list = function (cb) { cb(Object.keys(disks).map(Disk)) };
};

