// Route contact events
var RemoteEmitter = require('dnode/events');
var Access = require('./access');

module.exports = Contact;
Contact.prototype = new RemoteEmitter;

function Contact (from, to) {
    if (!(this instanceof Contact)) return new Contact(from, to);
    var self = this;
    
    self.name = to.name;
     
    self.online = to.connections > 0;
    
    to.on('online', function () {
        self.online = true;
        self.emit('online');
    });
    
    to.on('offline', function () {
        self.online = false;
        self.emit('offline');
    });
    
    self.message = function (msg) {
        if (!self.online) throw new Error(
            "Tried to message " + self.name + ", who is offline"
        )
        to.contacts[from.name].emit('message', msg);
    };
    
    self.share = function (type, id, rules) {
        var res = {
            process : function () { return from.processAt(id) },
            disk : function () { return from.disks[id] },
        }[type]();
        if (!res) throw new Error('No ' + type + ' at ' + id);
        
        to.share(res, function (tied) {
            var limited = tied.limit(to, Access(rules));
            if (limited) to.contacts[from.name].emit('share', type, limited);
        });
    };
};

