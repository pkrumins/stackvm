var DNode = require('dnode');

var engines = {
    qemu : require('./instance/qemu'),
    vmware : require('./instance/vmware'),
};

var ports = [];

module.exports = function Instance (params, cb) {
    var Engine = engines[params.disk.engine];
    
    var port = 5900;
    for (; port in ports; port++);
    params.port = port;
    
    Engine.spawn(params, function (inst) {
        ports.push(inst.port);
        inst.on('exit', function () {
            var i = ports.indexOf(inst.port);
            if (i >= 0) ports.slice(i,1);
        });
        cb(inst);
    });
}

