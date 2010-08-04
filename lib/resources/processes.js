// Manage processes

var EventEmitter = require('events').EventEmitter;
var Resource = require(__dirname + '/../resource');
var Process = require(__dirname + '/process');
 
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
        processes.forEach(function (proc) {
            proc.on('kill', function () {
                self.emit('kill', proc.addr);
            });
        });
    }
    
    return {
        bind : function (em, conn) {
            self.connections ++;
            self.resource.subscribe(em, conn);
            
            conn.on('end', function () {
                self.connections --;
                if (self.connections <= 0) {
                    delete Processes[user.name];
                }
            });
        },
    };
};

