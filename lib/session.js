var sys = require('sys');
var fs = require('fs');
var EventEmitter = require('events').EventEmitter;

var Resource = require('./resource');
var Chat = require('./resources/chat');
var FB = require('./resources/fb');

module.exports = Session;
function Session (params) {
    if (!(this instanceof Session)) return new Session(params);
    var self = this;
    
    self.user = params.user;
    self.user.contacts = self.user.contacts.map(function (name) {
        return {
            name : name,
            status : name in Session ? 'online' : 'offline'
        }
    });
    
    var conn = params.connection;
    var manager = params.manager;
    
    self.spawn = function (params, cb) {
        // todo: check permissions here
        manager.spawn({
            user : { name : self.user.name },
            disk : params.disk,
            engine : params.engine,
        }, cb);
    };
    
    self.kill = function (addr, cb) {
        // todo: check permissions here
        if (addr in FB) {
            FB[addr].end();
            manager.kill(addr, cb);
        }
    };
    
    self.restart = function (host, cb) {
        if (addr in FB) {
            FB[addr].end();
            manager.restart(addr, cb);
            // todo : resync procedure with client
        }
    };
    
    self.attach = function (addr, mode, em) {
        // todo: check permissions here
        // hard-coded qemu for now, pkrumins can fix that :p
        FB({ addr : addr, engine : 'qemu' }, function (fb) {
            fb.resources.encoder.subscribe(em, conn);
        });
    };
    
    self.detach = function (addr) {
        if (addr in FB) {
            FB[addr].resources.encoder.unsubscribe(conn);
        }
    };
    
    Session[self.user.name] = self;
    conn.on('end', function () {
        delete Session[self.user.name];
    });
};

