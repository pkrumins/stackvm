// Manage multiple connections to an RFB stream

var sys = require('sys');
var EventEmitter = require('events').EventEmitter;

var base64 = require('base64');
var Buffer = require('buffer').Buffer;
var PngLib = require('png');
var RFB = require('rfb').RFB;
var DNode = require('dnode').DNode;

VM.prototype = new EventEmitter;
exports.VM = VM;
function VM (opts) {
    var self = this;
    
    var pngStack = null;
    var screenshotRequests = 0;
    var screencastRequests = 0;
    var screencastId = null;
    var rfb = new RFB(opts || {});
    self.port = opts.port || 5900;
    
    var clients = {};
    
    self.attach = function (client_id) {
        var fb = new FB({
            rfb : rfb,
            encoder : encoder,
            inputEnabled : true,
        });
        fb.attach();
        clients[client_id] = fb;
        return fb;
    };
    
    self.detach = function (client_id) {
        var fb = clients[client_id];
        fb.detach();
        delete clients[client_id];
    };
}

