var Hash = require('traverse/hash');
var Store = require('supermarket');

module.exports = {
    qemu : require('./managers/qemu')(),
    vmware : require('./managers/vmware')(),
};

Store(__dirname + '/../../data/procs.db', function (err, procs) {
    if (err) throw err;

    Hash(module.exports).forEach(function (manager) {
        manager.on('spawn', function (proc) {
            procs.set(
                proc.address,
                JSON.stringify(Hash.extract(proc, 'address engine filename pid'.split(' '))),
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

    procs.filter(
        function () { return true },
        function (err, proc) {
            console.log('Connecting process ' + proc.pid + ' at ' + proc.address);
            var p = module.exports[proc.engine].connect(proc);
            if (!p) {
                console.log('Removing proccess ' + proc.pid + ' at ' + proc.address);
                procs.remove(
                    proc.address,
                    function (err) { if (err) throw err }
                );
            }
        }
    );
});

