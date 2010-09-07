var path = require('path');
var RemoteEmitter = require('dnode/events');
var Hash = require('traverse/hash');
var managers = require('../managers');

module.exports = Disk;
Disk.prototype = new RemoteEmitter;
function Disk (params) {
    if (!(this instanceof Disk)) return new Disk(params);
    var self = this;
    
    self.__proto__ = Hash.copy(self.__proto__);
    
    self.name = params.name;
    self.processes = {};
    var filename = params.filename;
    self.filename = path.basename(filename); // client sees just the basename
    
    // Connect to an existing process (hidden to clients)
    self.__proto__.connect = function (proc) {
        self.processes[proc.addr] = proc;
        
        proc.on('exit', function () {
            delete self.processes[proc.addr];
        });
    };
    
    self.on('attach', function (tied) {
        tied.tie('processes');
        
        // spawn a new vm process
        tied.spawn = function (engine) {
            managers[engine].spawn(filename, function (proc) {
                self.connect(proc);
                self.emit('spawn', tied.tie(proc));
            });
        };
        
    });
}
