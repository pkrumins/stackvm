var fs = require('fs');
var sys = require('sys');
var VM = require('./vm').VM;

var vmObjects = {};
var sessionId = 0;

exports.Session = function Session (params) {
    var self = this;
    
    var user = params.user;
    var client = params.client;
    var conn = params.connection;
    var manager = params.manager;
    
    var clientId = sessionId++;
    
    var cached = { vms : {}, procs : {} };
    var attached = {};
    
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
            if (!(port in vmObjects)) {
                vmObjects[port] = new VM({ port : port });
            }
            var fb = vmObjects[port].attach(clientId);
            attached[port] = fb;
            cb(fb);
        }
    };
    
    self.detach = function (port) {
        vmObjects[port].detach(clientId);
        delete attached[port];
    };
    
    conn.addListener('end', function () {
        attached.forEach(function (vm) {
            vm.detach(clientId);
            // could shutdown vm instance here if nobody is using it
        });
    });
};
