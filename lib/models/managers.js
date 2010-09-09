var Hash = require('traverse/hash');
var nStore = require('nStore');

var procs = nStore(__dirname + '/../../data/procs.db');

module.exports = {
    qemu : require('./managers/qemu')(),
    vmware : require('./managers/vmware')(),
};

Hash(module.exports).forEach(function (manager) {
    manager.on('spawn', function (proc) {
        procs.save(
            proc.address,
            Hash.extract(proc, 'address engine filename pid'.split(' ')),
            function (err) { if (err) throw err }
        );
    });
    
    manager.on('connect', function (proc) {
        proc.on('exit', function () {
            procs.remove(
                proc.address,
                function (err) { if (err) throw err }
            );
        });
    });
});

procs.stream().on('data', function (proc) {
    if (err) throw err;
    module.exports[proc.engine].connect(proc);
});
