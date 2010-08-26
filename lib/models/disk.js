var fs = require('fs');

var Model = require('../model');
var Instance = require('./instance');

module.exports = Model(Disk);
function Disk (params, session) {
    var self = this;
    self.subscribe = self.subscribe;
    
    self.name = params.name;
    self.mode = params.mode;
    
    var filename = params.directory + '/' + params.filename;
    
    var instances = {};
    self.instances = function (cb) { cb(instances) };
    
    // Copy this disk to the user's staging area.
    // The callback will be called repeatedly with the percentage complete.
    self.copy = function (user, dstFile, cb) {
        // r : copy an image to user's account
        if (!self.mode.forUser(user).r) { cb(null); return }
        if (dstFile.match(/\/|\.\./)) { cb(null); return } // -_-
        
        var rh = fs.createReadStream(filename);
        var wh = fs.createWriteStream(destFilename);
        
        rh.on('data', function (buf) {
            
        });
        sys.pump(rh, wh);
        
        cb(100);
    };
    
    self.spawn = function (user, cb) {
        // x : spawn an instance on the hoster's resources
        if (!self.mode.forUser(user).x) { cb(null); return }
        
        Instance(
            {
                engine : params.engine,
                filename : filename,
                host : 'localhost',
            },
            function (inst) {
                inst.subscribe = inst.subscribe;
                instances[inst.addr] = inst;
                
                inst.on('exit', function () {
                    self.emit('exit', inst);
                });
                
                if (cb) cb(inst);
                self.emit('spawn', inst);
            }
        );
    };
};

