var DNode = require('dnode');
var Model = require('../model');

var engines = {
    qemu : require('./instance/qemu'),
    vmware : require('./instance/vmware'),
};

var ports = [];

module.exports = Model(Instance)
function Instance (params, cb) {
    var Engine = engines[params.disk.engine];
    
    var port = 5900;
    for (; port in ports; port++);
    params.port = port;
    
    Engine.spawn(params, function (inst) {
        ports.push(inst.port);
        Framebuffer(inst, function (fb) {
            inst.on('exit', function () {
                var i = ports.indexOf(inst.port);
                if (i >= 0) ports.slice(i,1);
                fb.end();
            });
            inst.framebuffer = fb;
            cb(inst);
        });
    });
}

