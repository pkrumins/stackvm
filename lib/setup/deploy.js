var spawn = require('child_process').spawn;
var sys = require('sys');
var fs = require('fs');
var path = require('path');

var Hash = require('traverse/hash');
var Step = require('step');

module.exports = function deploy (base, opts, cb) {
    var self = this;
    var dirs = [ 'users', 'data', 'config' ];
    
    console.log('Deploying StackVM to ' + base);
    Step(
        function () { self.hasQemu(this) },
        function (hasQemu) { 
            if (!hasQemu) {
                cb('Qemu not detected in $PATH with `which qemu`. '
                    + ' If you still want to install, specify --no-qemu');
            }
            else {
                path.exists(base, this.parallel());
            }
        },
        function (hasBase) {
            if (!hasBase) fs.mkdir(base, 0700, this)
            else this(null);
        },
        function (err) {
            if (err) cb(err);
            else {
                dirs.forEach((function (dir) {
                    path.exists(
                        base + '/' + dir,
                        this.parallel().bind(this, null)
                    );
                }).bind(this));
            }
        },
        function (err) {
            var exists = Hash.zip(dirs, [].slice.call(arguments, 1));
            if (err) cb(err);
            else if (Hash.some(exists, Boolean)) {
                cb('Directories already exist in ' + base + ': '
                    + Hash(exists).filter(Boolean).keys.join(' ')
                )
            }
            else {
                dirs.forEach((function (dir) {
                    fs.mkdir(base + '/' + dir, 0700, this.parallel());
                }).bind(this));
            }
        },
        function (err) {
            if (err) cb(err);
            else cb(null);
        }
    );
};

function hasQemu (cb) {
    var which = spawn('which', ['qemu']);
    which.on('exit', function (code) {
        cb(code == 0);
    });
};
