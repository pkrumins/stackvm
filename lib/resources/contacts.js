// Route contact events

var EventEmitter = require('events').EventEmitter;
var Resource = require(__dirname + '/../resource');

module.exports = function Contacts (params) {
    
    var user = params.user;
    var contacts = params.contacts;
    var conn = params.connection;
    var em = params.emitter; // the remote emitter to push events out to
    
    function fromName (name) {
        return {
            name : name,
            online : name in Contacts,
            message : function (msg) {
                Contacts[name].emit('message', msg);
            },
        };
    }
    
    function broadcast (args) {
        contacts.forEach(function (name) {
            if (name in Contacts) {
                Contacts[name].emit.apply(Contacts[name], args);
            }
        });
    }
    
    var self = null;
    
    if (user.name in Contacts) {
        // sign-on while another session is active
        self = Contacts[user.name];
        c.emit('list', contacts.map(fromName));
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
};

