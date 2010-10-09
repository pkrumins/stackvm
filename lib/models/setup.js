var spawn = require('child_process').spawn;
var sys = require('sys');

exports.deploy = function (dir) {
    console.log('Deploying StackVM to ' + sys.inspect(dir));
};

exports.hasQemu = function (cb) {
    var which = spawn('which', ['qemu']);
    which.on('exit', function (code) {
        cb(code == 0);
    });
};
