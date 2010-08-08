// Route contact events
var EventEmitter = require('events').EventEmitter;
var DNode = require('dnode');

module.exports = function Contacts (params) {
    var user = params.user;
    var contacts = params.contacts;
    var conn = params.connection;
    var procs = params.processes;
    
    var self = new EventEmitter;
    
    function fromName (name) {
        var contact = {
            name : name,
            online : name in Contacts,
            message : function (msg) {
                if (name in Contacts) {
                    Contacts[name].emit('message', {
                        message : msg,
                        from : Contacts[name].fromName(user.name),
                    });
                }
            },
            share : function (addr, mode) {
                if (name in Contacts) {
                    procs.share({
                        addr : addr,
                        mode : mode,
                        to : Contacts[name],
                        from : Contacts[name].fromName(user.name)
                    });
                }
            },
        };
        return contact;
    }
    
    // send a message to all contacts
    function broadcast (ev, arg) {
        contacts.forEach(function (name) {
            if (name in Contacts) {
                Contacts[name].emit(ev, arg);
            }
        });
    }
    
    self.connections = 0;
    if (user.name in Contacts) {
        self = Contacts[user.name];
    }
    else {
        broadcast('online', user.name);
        self.fromName = fromName;
    }
    self.connections ++;
    
    conn.on('end', function () {
        self.connections --;
        if (self.connections <= 0) {
            delete Contacts[user.name];
            broadcast('offline', user.name);
        }
    });
    
    // todo: clean up the .on functions when the connection ends
    DNode.expose(self, 'on');
    self.list = function (cb) { cb(contacts.map(fromName)) };
    
    Contacts[user.name] = self;
    return self;
};

