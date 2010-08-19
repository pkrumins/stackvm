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
    
    function Contact (from, name) {
        this.name = name;
        this.online = name in Users;
        this.message = function (msg) {
            if (name in Users) {
                Users[name].emit('message', {
                    message : msg,
                    from : new Contact(name, from),
                });
            }
        };
    }
    
    self.contacts = function (cb) {
        var acc = {};
        contacts.forEach(function (name) {
            acc[name] = new Contact(user, name)
        });
        cb(acc);
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

