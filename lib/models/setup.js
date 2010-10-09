var spawn = require('child_process').spawn;
var sys = require('sys');
var fs = require('fs');
var path = require('path');

var Step = require('step');

exports.deploy = function (dir, opts, cb) {
    var self = this;
    console.log('Deploying StackVM to ' + dir);
    Step(
        function () { self.hasQemu(this) },
        function (err, hasQemu) { 
            if (err) cb(err);
            else if (!hasQemu) {
                cb('Qemu not detected in $PATH with `which qemu`. '
                    + ' If you still want to install, specify --no-qemu');
            }
            else {
                path.exists(dir, this.parallel());
                path.exists(dir + '/users', this.parallel());
                path.exists(dir + '/data', this.parallel());
            }
        },
        function mkdirs (err, root, users, data) {
            if (err) cb(err);
            else if (users[0]) {
                cb(dir + '/users directory already exists');
            }
            else if (data[0]) {
                cb(dir + '/data directory already exists');
            }
            else if (!root[0]) {
                fs.mkdir(dir, 0700, (function (err) {
                    mkdirs.bind(this)(err, [true], [false], [false]);
                }).bind(this));
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
