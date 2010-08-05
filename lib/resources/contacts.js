// Route contact events

var EventEmitter = require('events').EventEmitter;
var Resource = require('../resource');

module.exports = function Contacts (params) {
    
    var user = params.user;
    var contacts = params.contacts;
    
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
    
    return { bind : function (em, conn) {
        // push events out to this
         
        if (user.name in Contacts) {
            // sign-on while another session is active
            self = Contacts[user.name];
            em.emit('list', contacts.map(fromName));
        }
        else {
            // first sign-on
            self = new EventEmitter;
            self.resource = new Resource(self);
            self.connections = 0;
            Contacts[user.name] = self;
            broadcast('online', fromName(user.name));
        }
        
        self.connections ++;
        self.resource.subscribe(em, conn);
        
        conn.on('end', function () {
            self.connections --;
            if (self.connections <= 0) {
                broadcast('offline', fromName(user.name));
                delete Contacts[user.name];
            }
        });
    } };
};

