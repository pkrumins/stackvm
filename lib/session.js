var sys = require('sys');
var fs = require('fs');
var EventEmitter = require('events').EventEmitter;

var Resource = require('./resource');
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
    var manager = params.manager;
    
    /*
    Contacts({
        user : { name : self.user.name },
        contacts : self.user.contacts,
    }).bind(emitters.contacts, conn);
    
    Processes({
        user : { name : self.user.name },
        disks : self.user.disks,
        processes : Object.keys(self.user.processes)
            .reduce(function (acc,addr) {
                var proc = self.user.processes[addr];
                acc[addr] = new Process({
                    connection : conn,
                    proc : proc,
                    mode : 'rwx' // since we own these processes
                });
                return acc;
            }, {})
        ,
        manager : manager
    }).bind(emitters.processes, conn);
    */
};

