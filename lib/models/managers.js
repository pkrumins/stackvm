var Hash = require('traverse/hash');
var Store = require('supermarket');

module.exports = {
    qemu : require('./managers/qemu')(),
    vmware : require('./managers/vmware')(),
};

Store({ filename : __dirname + '/../../data/procs.db', json : true }, function (err, procs) {
    if (err) throw err;

    Hash(module.exports).forEach(function (manager) {
        manager.on('spawn', function (proc) {
            procs.set(
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

    procs.forEach(function (proc) {
        console.log('Connecting process ' + proc.value.pid + ' at ' + proc.value.address);
        var p = module.exports[proc.engine].connect(proc);
        if (!p) {
            console.log('Removing proccess ' + proc.value.pid + ' at ' + proc.value.address);
            procs.remove(
                proc.value.address,
                function (err) { if (err) throw err }
            );
        }
    });
});

