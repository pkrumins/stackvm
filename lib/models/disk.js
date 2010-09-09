var path = require('path');
var RemoteEmitter = require('dnode/events');
var Hash = require('traverse/hash');

var managers = require('./managers');
var Access = require('./access');

module.exports = Disk;
Disk.prototype = new RemoteEmitter;
function Disk (params) {
    if (!(this instanceof Disk)) return new Disk(params);
    var self = this;
    
    // DNode clients can't see or call stuff in __proto__.
    self.__proto__ = Hash.copy(self.__proto__);
    
    self.name = params.name;
    self.processes = {};
    var filename = params.filename;
    self.filename = path.basename(filename); // client sees just the basename
    
    // Connect to an existing process
    self.__proto__.connect = function (proc) {
        self.processes[proc.address] = proc;
        proc.on('exit', function () {
            delete self.processes[proc.address];
        });
    };
    
    self.__proto__.access = Access(params.rules || {});
    
    self.__proto__.limit = function (user, access) {
        var can = (access ? access : self.access).allowed(user);
        
        var share = Hash.copy(this);
        if (!can.copy) delete share.copy;
        if (!can.spawn) delete share.spawn;
        
        share.processes = Hash(share.processes)
            .map(function (proc) {
                var pcan = proc.access.allowed(user);
                if (pcan) return proc.limit(user);
            })
            .filter(function (x) { return x !== undefined })
            .items
        ;
        
        return share;
    };
    
    self.on('attach', function (tied) {
        tied.tie('processes');
        
        // spawn a new vm process
        tied.spawn = function (engine) {
            managers[engine].spawn(filename, function (proc) {
                proc.name = self.name;
                self.emit('spawn', tied.tie(proc));
            });
        };
        
        tied.copy = function (dstFile) {
            console.log('copy not implemented... yet');
        };
    });
}
