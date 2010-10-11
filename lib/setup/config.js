var fs = require('fs');
var path = require('path');
var EventEmitter = require('events').EventEmitter;
var Step = require('step');
var Hash = require('traverse/hash');

var mkdirP = require('./mkdir_p');

module.exports = function (cb) {
    var configDir = process.env.HOME + '/.config/stackvm';
    var localFile = configDir + '/local.json';
    var remoteFile = configDir + '/remote.json';
    
    Step(
        function () { mkdirP(configDir, 0700, this) },
        function (err) {
            path.exists(localFile, this.parallel().bind(this, null));
            path.exists(remoteFile, this.parallel().bind(this, null));
        }, 
        function (err, lex, rex) {
            if (err) { cb(err); return }
            
            var ex = Hash([ localFile, remoteFile ], [ lex, rex ])
                .filter(function (x) { return !x });
            
            ex.forEach((function (x, file) {
                var ws = fs.createWriteStream(file, { mode : 0600 });
                ws.on('close', this.parallel().bind(this, null));
                ws.write(JSON.stringify({}));
                ws.end();
            }).bind(this));
            
            if (ex.length == 0) this();
        },
        function (err) {
            if (err) cb(err)
            else {
                ConfigFile(localFile, this.parallel());
                ConfigFile(remoteFile, this.parallel());
            }
        },
        function (err, local, remote) {
            if (err) cb(err)
            cb(null, { local : local, remote : remote });
        }
    );
};

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
