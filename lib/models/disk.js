var RemoteEmitter = require('../remote');
var Process = require('./process');
var managers = require('./managers');
var DNode = require('dnode');

Disk.prototype = new RemoteEmitter;
function Disk (params) {
    var self = this;
    
    DNode.expose(self, 'subscribe');
    
    self.name = params.name;
    var filename = params.user.directories.disks + '/' + params.filename,
    
    self.mode = params.mode;
    
    var instances = {};
    self.instances = function (cb) { cb(instances) };
    
    self.spawn = function (cb) {
        Instance(
            {
                engine : params.engine,
                filename : filename,
                host : 'localhost',
            },
            function (inst) {
                DNode.expose(inst, 'subscribe');
                instances[inst.addr] = inst;
                
                inst.on('exit', function () {
                    self.emit('exit', inst);
                });
                
                if (cb) cb(inst);
                self.emit('spawn', inst);
            }
        );
    };
};

