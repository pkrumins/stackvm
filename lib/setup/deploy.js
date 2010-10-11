var spawn = require('child_process').spawn;
var sys = require('sys');
var Hash = require('traverse/hash');
var Step = require('step');

var Config = require('./config.js');
var mkdirP = require('./mkdir_p');

exports.deploy = function (opts) {
    var self = this;
    var dirs = [ 'users', 'data', 'logs' ];
    var cb = opts.done;
    
    Step(
        function () { self.hasQemu(this) },
        function (hasQemu) { 
            if (!hasQemu && opts.qemu != false) {
                cb('Qemu not detected in $PATH with `which qemu`. '
                    + ' If you still want to install, specify --no-qemu');
            }
            else {
                dirs.forEach((function (dir) {
                    mkdirP(opts.base + '/' + dir, 0700, this.parallel());
                }).bind(this));
            }
        },
        function (err) {
            if (err) cb(err)
            else Config(function (err, config) {
                if (err) cb(err)
                else config.local
                    .update(function (data) {
                        var p = opts.port || 9000;
                        while (Hash(data).some(function (x) {
                            return x.port == p
                        })) p++;
                        
                        data[opts.name] = {
                            directory : opts.base,
                            port : p,
                            pid : null,
                        };
                    })
                    .on('update', cb.bind({}, null))
            })
        }
    );
};

function hasQemu (cb) {
    var which = spawn('which', ['qemu']);
    which.on('exit', function (code) {
        cb(code == 0);
    });
};

exports.undeploy = function (name, cb) {
    Config(function (err, config) {
        if (err) cb(err)
        else config.local
            .update(function (data) { delete data[name] })
            .on('update', cb.bind({}, null))
    })
};
