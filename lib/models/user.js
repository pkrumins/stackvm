var DNode = require('dnode');
var crypto = require('crypto');
var sys = require('sys');

var Model = require('../model');
var Disk = require('./disk');
var Mode = require('./mode');

var users = {};

module.exports = Model(User);
function User (params) {
    if (!(this instanceof User)) return new User(params);
    var self = this;
    
    self.name = params.key.toLowerCase();
    users[self.name] = self;
    
    var hash = params.hash;
    self.subscribe = self.subscribe;
    
    var contacts = params.contacts;
    
    self.directory = __dirname + '/../../users/' + self.name;
    
    self.directories = {
        disks : self.directory + '/disks',
    };
    
    self.groups = params.groups;
    
    // the groups that a user belongs to
    self.groupsFor = function (u) {
        return Object.keys(self.groups).filter(function (g) {
            return self.groups[g].indexOf(u.name) >= 0;
        });
    };
    
    var session = { user : self, users : users };
    self.disks = Disk.load(params.disks, session);
    
    function Contact (from, name) {
        this.name = name;
        this.online = name in users;
        this.message = function (msg) {
            if (name in users) {
                users[name].emit('message', {
                    text : msg,
                    from : new Contact(name, from),
                });
            }
        };
    }
    
    self.contacts = function (cb) {
        var acc = {};
        contacts.forEach(function (name) {
            acc[name] = new Contact(self.name, name)
        });
        cb(acc);
    };
    
    self.authenticate = function (pass, cb) {
        // emit _online and _offline outside of this call to support seamless
        // multiple sign-on
        cb(hash == User.hash(pass) ? self : null);
    };
    
    // stuff for after the user list has been built up
    self.finalize = function () {
        // listen for contact status changes
        contacts.forEach(function (name) {
            users[name].on('_online', function () {
                self.emit('online', name);
            });
            
            users[name].on('_offline', function () {
                self.emit('offline', name);
            });
            
        });
        
        Object.keys(self.disks).forEach(function (filename) {
            var disk = self.disks[filename];
            disk.mode = new Mode(
                self, self.disks[disk.filename].mode
            );
        });
        
        delete self.finalize;
    };
}

User.hash = function (phrase) {
    return new crypto.Hash('sha512').update(phrase).digest('hex');
};

