var sys = require('sys');
var fs = require('fs');
var EventEmitter = require('events').EventEmitter;

var FB = require('./fb').FB;

var desktops = {};
var users = {};

module.exports = Session;
Session.prototype = new EventEmitter;
function Session (params) {
    if (!(this instanceof Session)) return new Session(params);
    var self = this;
    
    // expose prototype methods so dnode can see them:
    self.on = self.on;
    
    // pass an event along to another emitter
    self.pass = function (name, em) {
        self.on(name, function () {
            var args = [].slice.apply(arguments);
            args.unshift(name);
            self.emit.apply(em, args);
        });
    };
    
    var user = params.user;
    users[user.name] = user;
    
    var client = params.client;
    var conn = params.connection;
    var manager = params.manager;
    
    var cached = { vms : {}, procs : {} };
    var connections = [];
    
    self.instances = function (cb) {
        self.virtualMachines(function (vms) {
            self.processes(function (procs) {
                Object.keys(vms).forEach(function (id) {
                    var vm = vms[id];
                    vm.instances = Object.keys(procs).map(function (pId) {
                        return procs[pId];
                    }).filter(function (proc) {
                        return proc.vm == vm.id;
                    });
                });
                cb(vms);
            });
        });
    };
    
    self.virtualMachines = function (cb) {
        manager.virtualMachines(user, function (vms) {
            cached.vms = vms;
            cb(vms);
        });
    };
    
    self.processes = function (cb) {
        manager.processes(user, function (procs) {
            cached.procs = procs;
            cb(procs);
        });
    };
    
    self.spawn = function (vm, engine, cb) {
        if (!(vm.id in cached.vms)) {
            cb(null);
        }
        else {
            manager.spawn(
                {
                    user : user,
                    vm : cached.vms[vm.id],
                    engine : engine,
                },
                function (proc) {
                    cached.procs[proc.host] = proc;
                    self.emit('spawn', vm, proc);
                    cb(proc)
                }
            );
        }
    };
    
    self.kill = function (host, cb) {
        connections
            .filter(function (h) { return h == host })
            .forEach(function (h) { self.detach(host) })
        ;
        manager.kill(user, host, cb);
        self.emit('kill', host);
    };
    
    self.restart = function (host, cb) {
        console.log('restart not impelmented');
        // manager.restart(user, host, cb);
        self.emit('restart', host);
    };
    
    self.attach = function (host,cb) {
        if (!(host in cached.procs)) {
            cb(null);
        }
        else {
            if (!(host in desktops)) {
                var fb = new FB({
                    host : host,
                    inputEnabled : true,
                    engine : cached.procs[host].engine
                });
                
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
    
    self.chat = function (f) {
        var chat = new Chat({
        });
        
        chat.on('msg', function () {
        });
        
        chat.on('invite', function () {
        });
        
        users[user.name].chat = chat;
        
        f(chat);
    };
    
    conn.on('end', function () {
        connections.forEach(function (host) { self.detach(host) });
        delete users[user.name];
    });
};
