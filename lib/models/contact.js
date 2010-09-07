// Route contact events
var RemoteEmitter = require('dnode/events');

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
    
    self.share = function (addr, mode) {
        to.contacts[from.name].emit(
            'share', from.disks[addr].share(mode)
        );
    };
};

