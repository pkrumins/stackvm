var fs = require('fs');

exports.Session = function Session (params) {
    var user = params.user;
    var client = params.client;
    var conn = params.connection;
    
    var self = this;
    var path = {
        vms : __dirname + '/../users/' + user + '/vms'
    };
    var attached = {};
    
    this.vmList = function (cb) {
        fs.readdir(path.vms, function (err,vms) {
            cb(vms ? vms : []);
        });
    };
    
    this.attach = function (vmName,cb) {
        // cb(new VM)
    };
    
    this.detach = function (vmName) {
    };
    
    conn.addListener('disconnect', function () {
        Object.keys(attached).forEach(function (vmName) {
            self.detach(vmName);
        });
    });
};
