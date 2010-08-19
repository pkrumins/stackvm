var RemoteEmitter = require('../remote');
var DNode = require('dnode');

var Users = {};
User.prototype = new RemoteEmitter;
function User (params) {
    var self = this;
    self.name = params.name;
    Users[self.name] = self;
    var contacts = params.contacts;
    
    DNode.expose(self, 'subscribe');
    
    self.directory = __dirname + '/../../users/' + self.name;
    
    self.directories = {
        disks : self.directory + '/disks',
    };
    
    var connections = 0;
    self.attach = function (conn) {
        if (connections == 0) self.emit('online');
        
        connections ++;
        conn.on('end', function () {
            connections --;
            if (connections <= 0) {
                self.emit('offline');
            }
        });
        
        contacts.forEach(function (name) {
            Users[name].subscribe(function () {
                this.on('online', function () {
                    self.emit('online', name);
                });
                this.on('offline', function () {
                    self.emit('offline', name);
                });
            });
        });
    };
}

