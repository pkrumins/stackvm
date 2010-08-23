var RemoteEmitter = require('../remote');
var DNode = require('dnode');
var crypto = require('crypto');
var sys = require('sys');

module.exports = User;
User.prototype = new RemoteEmitter;
function User (params) {
    if (!(this instanceof User)) return new User(params);
    var self = this;
    self.name = params.name.toLowerCase();
    var hash = params.hash;
    self.subscribe = self.subscribe;
    
    var users = params.users;
    var contacts = params.contacts;
    
    self.directory = __dirname + '/../../users/' + self.name;
    
    self.directories = {
        disks : self.directory + '/disks',
    };
    
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
        delete self.finalize;
    };
}

User.fromList = function (list) {
    var users = {};
    list.forEach(function (params) {
        var user = new User({
            name : params.name,
            contacts : params.contacts,
            users : users,
        });
        users[params.name] = user;
    });
    list.forEach(function (params) {
        users[params.name].finalize();
    });
    return users;
};

User.hash = function (phrase) {
    return new crypto.Hash('sha512').update(phrase).digest('hex');
};

