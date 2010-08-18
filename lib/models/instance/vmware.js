var net = require('net');
var RemoteEmitter = require('../../remote');

module.exports = VMWare;

VMWare.spawn = function (params, cb) {
    cb(new VMWare(params));
};

VMWare.prototype = new RemoteEmitter;

function VMWare (params) {
    var self = this;
    self.addr = params.host + ':' + params.port;
    self.port = params.port;
    
    self.alive = function (cb) {
        var stream = net.createConnection(params.port, params.host);
        stream.setTimeout(1000);
        stream.on('connect', function () {
            stream.end();
            cb(true);
        });
        stream.on('error', function () {
            cb(false);
        });
    };
    
    self.kill = function (cb) { cb(true) };
}

