// Never give the user direct access to this object, use session instead

var EventEmitter = require('events').EventEmitter;
var crypto = require('crypto');
var Hash = require('traverse/hash');

var Disk = require('models/disk');
var Contact = require('models/contact');

// all manners of hashes in this file

module.exports = User;

User.prototype = new EventEmitter;
function User (params) {
    if (!(this instanceof User)) return new User(params);
    var self = this; 
    
    self.sessions = {};
    self.everyone = {};
    self.connections = 0;
    
    self.name = params.name;
    self.hash = params.hash;
    
    self.directories = {
        disks : __dirname + '/../../users/' + self.name + '/disks/',
    };
    
    self.disks = Hash.map(params.disks, function (disk, filename) {
        disk.filename = self.directories.disks + filename;
        return new Disk(disk);
    });
    
    self.proccessAt = function (addr) {
        return Hash(self.disks)
            .filter(function (disk) {
                return disk.processes.hasOwnProperty(addr);
            })
            .map(function (disk) {
                return disk.processes[addr];
            }).values[0];
    };
    
    self.share = function (res, cb) {
        Hash(self.sessions).forEach(function (session) {
            cb(res.attach(session.connection));
        });
    };
}

User.hash = function (phrase) {
    return new crypto.Hash('sha512').update(phrase).digest('hex');
};

User.fromHashes = function (hashes) {
    var users = Hash.map(hashes, function (user, name) {
        user.name = name;
        return new User(user);
    });
    
    Hash(users).forEach(function (user, name) {
        user.contacts = {};
        hashes[name].contacts.forEach(function (name) {
            user.contacts[name] = new Contact(user, users[name]);
        });
        user.everyone = users;
    });
    
    return users;
};
