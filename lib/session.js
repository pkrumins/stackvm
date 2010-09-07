var RemoteEmitter = require('dnode/events');

module.exports = Session;
Session.prototype = new RemoteEmitter;
function Session (user) {
    if (!(this instanceof Session)) return new Session(params);
    var self = this;
    self.name = user.name;
    self.disks = user.disks;
    self.contacts = user.contacts;
    
    self.on('attach', function (attached) {
        attached.tie('disks');
        attached.tie('contacts');
    });
};

