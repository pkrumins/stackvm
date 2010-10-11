var fs = require('fs');
var path = require('path');
var EventEmitter = require('events').EventEmitter;
var Step = require('step');
var Hash = require('traverse/hash');

var mkdirP = require('./mkdir_p');

module.exports = function (cb) {
    var configDir = process.env.HOME + '/.config/stackvm';
    var files = [ 'local', 'remote' ].reduce(function (acc, x) {
        acc[x] = configDir + '/' + x + '.json';
        return acc;
    }, {});
    
    Step(
        function () { mkdirP(configDir, 0700, this) },
        function (err) {
            Hash(files).forEach((function (file) {
                path.exists(file, this.parallel().bind(this, null));
            }).bind(this));
        }, 
        function (err) {
            if (err) { cb(err); return }
            
            this.parallel()(null);
            
            Hash(Object.keys(files), [].slice.call(arguments, 1))
                .filter(function (x) { return !x })
                .forEach((function (x, file) {
                    var filename = configDir + '/' + file + '.json';
                    var ws = fs.createWriteStream(filename, { mode : 0600 });
                    ws.on('close', this.parallel().bind(this, null));
                    ws.write(JSON.stringify({}));
                    ws.end();
                }).bind(this))
            ;
        },
        function (err) {
            if (err) cb(err)
            else {
                Hash(files).forEach((function (file) {
                    ConfigFile(file, this.parallel());
                }).bind(this));
            }
        },
        function (err) {
            if (err) cb(err)
            else cb(null, Hash.zip(
                Object.keys(files), [].slice.call(arguments, 1)
            ));
        }
    );
};

exports.ConfigFile = ConfigFile;
function ConfigFile (filename, cb) {
    var self = new EventEmitter;
    self.update = function (f) {
        fs.readFile(filename, function (err, data) {
            var hash = JSON.parse(data);
            var result = f(hash);
            var updated = result === undefined ? hash : result;
            var ws = fs.createWriteStream(filename, { mode : 0600 });
            ws.on('close', self.emit.bind(self, 'update', updated));
            ws.write(JSON.stringify(updated));
            ws.end();
        });
        return self;
    };
    
    fs.readFile(filename, function (err, data) {
        if (err) cb(err);
        else cb(null, Hash.merge(
            self, { data : JSON.parse(data) }
        ));
    });
}
