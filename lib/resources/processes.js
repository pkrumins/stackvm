// Manage processes

var RemoteEmitter = require('dnode/events');
var Hash = require('traverse/hash');

var Process = require('./process');
var DNode = require('dnode');
var managers = require('../managers.js');
 
module.exports = Processes;
Processes.prototype = new RemoteEmitter;
function Processes (params) {
    if (!(this instanceof Processes)) return new Processes(params);
    var self = this;
    
    var user = params.user;
    var disks = user.disks;
    
    var processes = {};
    
    self.share = function (args) {
        var proc = processes[args.addr];
        proc.attach(function (fb) {
            args.to.emit('share', {
                name : proc.name,
                from : args.from,
                mode : args.mode,
                fb : {
                    encoder : args.mode.indexOf('r') >= 0
                        ? fb.encoder : null,
                    input : args.mode.indexOf('w') >= 0
                        ? fb.input : null,
                    size : fb.size,
                }
            });
        });
    };
    
    self.list = function (cb) { cb(Object.keys(disks).map(Disk)) };
};

