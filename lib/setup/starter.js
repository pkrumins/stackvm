var fs = require('fs');
var spawn = require('child_process').spawn;
var Step = require('step');
var Config = require('./config');

exports.start = function (name, cb) {
    Config(function (err, config) {
        if (err) { cb(err); return }
        
        var params = config.local.data[name];
        if (!params) { cb('Unknown name: ' + name); return }
        var dir = params.directory;
        
        fs.open(dir + '/logs/' + Date.now(), 'w', function (err, logFd) {
            var server = spawn('node',
                [ __dirname + '/../../bin/server.js', params.port ],
                { cwd : dir, customFds : [ -1, logFd, logFd ] }
            );
            config.local.update(function (data) {
                data[name].pid = server.pid;
            });
        });
    });
};

exports.stop = function (name, cb) {
    Config(function (err, config) {
        if (err) { cb(err); return }
        var params = config.local.data[name];
        if (!params) { cb('Unknown name: ' + name); return }
        console.log('pid = ' + params.pid);
        cb(null);
    });
};
