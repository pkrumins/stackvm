var sys = require('sys');
var fs = require('fs');
var FB = require('./fb').FB;

var desktops = {};

module.exports = function Session (params) {
    if (!(this instanceof Session)) return new Session(params);
    var self = this;
    
    var user = params.user;
    var client = params.client;
    var conn = params.connection;
    var manager = params.manager;
    
    var cached = { vms : {}, procs : {} };
    var connections = [];
    
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
    
    self.spawn = function (params, cb) {
        if (!(params.vm in cached.vms)) {
            cb(null);
        }
        else {
            manager.spawn(
                {
                    user : user,
                    vm : cached.vms[params.vm],
                    engine : params.engine,
                },
                function (proc) {
                    cached.procs[proc.host] = proc;
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
                });
                var fb_emit = fb.emit;
                delete fb.emit; // so we can emit but the browser can't
                
                desktops[host] = {
                    fb : fb,
                    clients : 0,
                    size : { width : 0, height : 0 },
                };
                
                // memoize the desktop size accross connections so clients that
                // connect won't need to recompute this value themselves
                fb.dimensions(function (size) {
                    console.dir(size);
                    fb_emit('desktopSize', size);
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
        }
    };
    
    self.detach = function (host) {
        var i = connections.indexOf(host);
        if (i >= 0) {
            connections.splice(i,1);
            var desktop = desktops[host];
            desktop.clients --;
            /*
            // defeats the purpose of memoizing size:
            if (desktop.clients <= 0) {
                desktop.fb.detach();
                delete desktops[host];
            }
            */
        }
    };
    
    conn.on('end', function () {
        connections.forEach(function (host) { self.detach(host) });
    });
};
