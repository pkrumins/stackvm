var sys = require('sys');
var fs = require('fs');
var EventEmitter = require('events').EventEmitter;

var Contacts = require('./resources/contacts');
var Processes = require('./resources/processes');
var Process = require('./resources/process');
var FB = require('./resources/fb');

module.exports = Session;
function Session (params) {
    if (!(this instanceof Session)) return new Session(params);
    var self = this;
    
    self.user = params.user;
    
    var conn = params.connection;
    var client = params.client;
    
    self.processes = new Processes({
        user : { name : self.user.name },
        disks : self.user.disks,
        processes : self.user.processes,
        connection : conn,
    });
    
    self.contacts = new Contacts({
        user : { name : self.user.name },
        contacts : self.user.contacts,
        connection : conn,
        processes : self.processes,
    });
    
    self.processes.on('exit', function (addr) {
        delete self.user.processes[addr];
    });
};

