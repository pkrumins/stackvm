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
    this.connections = 0;
    this.name = params.name;
    this.hash = params.hash;
    
    this.disks = Hash.map(params.disks, function (disk, filename) {
        disk.filename = filename;
        return new Disk(disk);
    });
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
    });
    
    return users;
};
