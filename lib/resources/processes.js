// Manage processes

var EventEmitter = require('events').EventEmitter;
var Resource = require(__dirname + '/../resource');
var Process = require(__dirname + '/process');
 
module.exports = function Processes (params) {
    var user = params.user;
    var disks = params.disks;
    var manager = params.manager;
    
    var self = null;
    
    manager.kill(addr, cb);
    
    return { bind : function (em, conn) {
        // push events out to this
         
        if (user.name in Contacts) {
            // sign-on while another session is active
            self = Contacts[user.name];
            self.emit('list', contacts.map(fromName));
        }
        else {
            // first sign-on
            self = new EventEmitter;
            self.resource = new Resource(self);
            self.connections = 0;
            Contacts[user.name] = self;
            broadcast('online', user.name);
        }
        
        self.connections ++;
        self.resource.subscribe(em, conn);
        
        conn.on('end', function () {
            self.connections --;
            if (self.connections <= 0) {
                broadcast('offline', user.name);
                delete Contacts[user.name];
            }
        });
    } };
};

