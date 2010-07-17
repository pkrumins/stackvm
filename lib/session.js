var fs = require('fs');
var sys = require('sys');
var VM = require('./vm').VM;

var vms = {};
var sessionId = 0;

exports.Session = function Session (params) {
    var user = params.user;
    var client = params.client;
    var conn = params.connection;
    var clientId = sessionId++;
    
    var self = this;
    var path = {
        vms : __dirname + '/../users/' + user + '/vms'
    };
    var attached = {};
    
    this.vmList = function (cb) {
        fs.readdir(path.vms, function (err,list) {
            cb(list ? list : []);
        });
    };
    
    this.attach = function (vmName,cb) {
    
        if (!(vmName in vms)) {
            var ports = Object.keys(vms).map(function (name) {
                return vms[name].port;
            });
            
            var port;
            for (port = 5900; ports.indexOf(port) >= 0; port++);
            
            //qemu.spawn(vmName, port);
            
            vms[vmName] = new VM({
                id : vmName,
                port : port,
            });
        }
        
        // Note: VM exposes some EventEmitter to the client.
        // Clients can use these to mess stuff up for other people who are
        // connected.
        cb(vms[vmName]);
    };
    
    conn.addListener('disconnect', function () {
        Object.keys(attached).forEach(function (vmName) {
            vms[vmName].detach(clientId);
            // could shutdown vm instance here if nobody is using it
        });
    });
};
