var sys = require('sys');
var fs = require('fs');
var FB = require('./fb').FB;

var frameBuffers = {};
var attached = {};

module.exports = function Session (params) {
    if (!(this instanceof Session)) return new Session(params);
    var self = this;
    
    var user = params.user;
    var client = params.client;
    var conn = params.connection;
    var manager = params.manager;
    
    var cached = { vms : {}, procs : {} };
    var ports = [];
    
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
        manager.kill(user, port, cb);
    };
    
    self.attach = function (port,cb) {
        if (!(port in cached.procs)) {
            cb(null);
        }
        else {
            if (!(port in frameBuffers)) {
                frameBuffers[port] = new FB({
                    port : port,
                    inputEnabled : true,
                });
                attached[port] = 0;
            }
            var fb = frameBuffers[port];
            fb.attach();
            attached[port] ++;
            ports.push(port);
            cb(fb);
        }
    };
    
    self.detach = function (port) {
        var i = ports.indexOf(port);
        if (i >= 0) {
            ports.splice(i,1);
            var fb = frameBuffers[port];
            attached[port] --;
            if (attached[port] <= 0) {
                fb.detach();
                delete frameBuffers[port];
            }
        }
    };
    
    conn.on('end', function () {
        ports.forEach(function (port) { self.detach(port) });
    });
};
