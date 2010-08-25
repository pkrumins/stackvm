var RemoteEmitter = require('../remote');
var Instance = require('./instance');

Disk.prototype = new RemoteEmitter;
function Disk (params) {
    var self = this;
    self.subscribe = self.subscribe;
    
    self.name = params.name;
    var filename = params.directory + '/' + params.filename;
    
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
                inst.subscribe = inst.subscribe;
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

