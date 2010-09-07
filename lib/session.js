var RemoteEmitter = require('dnode/events');
var Hash = require('traverse/hash');

module.exports = Session;
Session.prototype = new RemoteEmitter;
function Session (user, conn) {
    if (!(this instanceof Session)) return new Session(params);
    var self = this;
    self.__proto__ = Hash.copy(self.__proto__);
    self.__proto__.connection = conn;
    
    self.name = user.name;
    self.disks = user.disks;
    self.contacts = user.contacts;
    
    self.on('attach', function (attached) {
        attached.tie('disks');
        attached.tie('contacts');
    });
};

