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
                    cached.procs[proc.port] = proc;
                    cb(proc)
                }
            );
        }
    };
    
    self.kill = function (port, cb) {
        connections
            .filter(function (p) { return p == port })
            .forEach(function (p) { self.detach(p) })
        ;
        manager.kill(user, port, cb);
    };
    
    self.attach = function (port,cb) {
        if (!(port in cached.procs)) {
            cb(null);
        }
        else {
            if (!(port in desktops)) {
                var fb = new FB({
                    port : port,
                    inputEnabled : true,
                });
                
                desktops[port] = {
                    fb : fb,
                    clients : 0,
                    size : { width : 0, height : 0 },
                };
                
                // memoize the desktop size accross connections so clients that
                // connect won't need to recompute this value themselves
                function desktopSize (size) {
                    desktops[port].size = size;
                }
                
                fb.dimensions(desktopSize);
                fb.on('desktopSize', desktopSize);
            }
            var desktop = desktops[port];
            desktop.fb.attach();
            desktop.clients ++;
            connections.push(port);
            cb(desktop);
        }
    };
    
    self.detach = function (port) {
        var i = connections.indexOf(port);
        if (i >= 0) {
            connections.splice(i,1);
            var desktop = desktops[port];
            desktop.clients --;
            if (desktop.clients <= 0) {
                desktop.fb.detach();
                delete desktops[port];
            }
        }
    };
    
    conn.on('end', function () {
        connections.forEach(function (port) { self.detach(port) });
    });
};
