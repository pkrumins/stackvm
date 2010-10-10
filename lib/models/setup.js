var spawn = require('child_process').spawn;
var sys = require('sys');
var fs = require('fs');
var path = require('path');

var Hash = require('traverse/hash');
var Step = require('step');

exports.deploy = function (dir, opts, cb) {
    var self = this;
    console.log('Deploying StackVM to ' + dir);
    Step(
        function () { self.hasQemu(this) },
        function (hasQemu) { 
            if (!hasQemu) {
                cb('Qemu not detected in $PATH with `which qemu`. '
                    + ' If you still want to install, specify --no-qemu');
            }
            else {
                path.exists(dir, this.parallel());
            }
        },
        function (hasRoot) {
            if (!hasRoot) fs.mkdir(dir, 0700, this)
            else this(null);
        },
        function (err) {
            if (err) cb(err);
            path.exists(dir + '/users', this.parallel().bind(this, null));
            path.exists(dir + '/data', this.parallel().bind(this, null));
        },
        function (err, users, data) {
            if (err) cb(err);
            else if (users || data) {
                cb('Directories already exist in ' + dir + ': '
                    + Hash(['users','data'],[users,data])
                        .filter(Boolean).keys.join(' ')
                )
            }
            else {
                fs.mkdir(dir + '/users', 0700, this.parallel());
                fs.mkdir(dir + '/data', 0700, this.parallel());
            }
        },
        function (err) {
            if (err) cb(err);
            else cb(null);
        }
    );
};

exports.hasQemu = function (cb) {
    var which = spawn('which', ['qemu']);
    which.on('exit', function (code) {
        cb(code == 0);
    });
};
