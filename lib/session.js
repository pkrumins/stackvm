var sys = require('sys');
var fs = require('fs');
var EventEmitter = require('events').EventEmitter;

var FB = require('./fb');
var Chat = require('./chat');

var desktops = {};

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
    
    var client = params.client;
    var conn = params.connection;
    var manager = params.manager;
    
    var connections = [];
    
    self.spawn = function (params, cb) {
        var disk = params.disk;
        var engine = params.engine;
        
        if (!(disk in self.user.disks)) {
            if (cb) cb(null);
        }
        else {
            var params = {
                user : { name : self.user.name },
                disk : disk,
                engine : engine,
            };
            manager.spawn(params, function (proc) {
                self.user.processes[proc.addr] = proc;
                if (cb) cb(proc)
            });
        }
    };
    
    self.kill = function (addr, cb) {
        connections
            .filter(function (a) { return a == addr })
            .forEach(function (a) { self.detach(addr) })
        ;
        manager.kill(self.user.processes[addr], cb);
    };
    
    self.restart = function (host, cb) {
        console.log('restart not impelmented');
        // manager.restart(self.user, host, cb);
        self.emit('restart', host);
    };
    
    self.attach = function (addr, cb) {
        if (!(addr in self.user.processes)) {
            cb(null);
        }
        else {
            if (!(addr in desktops)) {
                var fb = new FB({
                    addr : addr,
                    inputEnabled : true,
                    engine : cached.procs[host].engine
                });
                DNode.expose(fb, 'on');
                
                desktops[host] = {
                    fb : fb,
                    clients : 0,
                    size : { width : 0, height : 0 },
                };
                
                // memoize the desktop size accross connections so clients that
                // connect won't need to recompute this value themselves
                fb.dimensions(function (size) {
                    console.dir(size);
                    fb.emit('desktopSize', size);
                    desktops[host].size = size;
                });
                
                fb.on('desktopSize', function (size) {
                    desktops[host].size = size;
                });
            }
            var desktop = desktops[host];
            desktop.fb.attach();
            desktop.clients ++;
            connections.push(host);
            cb(desktop);
            self.emit('attach', host);
        }
    };
    
    self.detach = function (host) {
        var i = connections.indexOf(host);
        if (i >= 0) {
            connections.splice(i,1);
            var desktop = desktops[host];
            desktop.clients --;
            if (desktop.clients <= 0) {
                desktop.fb.detach();
                delete desktops[host];
            }
            self.emit('detach', host);
        }
    };
    
    self.chat = new Chat({
        user : self.user,
        connection : conn,
    });
    
    Session[self.user.name] = self;
    conn.on('end', function () {
        connections.forEach(function (host) { self.detach(host) });
        delete Session[self.user.name];
    });
};

