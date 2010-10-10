var path = require('path');
var Traverse = require('traverse');
var Hash = require('traverse/hash');
var Step = require('step');

module.exports = function (data, cb) {
    if (typeof cb != 'function') {
        throw new Error('Expecting function for callback, got: ' + typeof cb);
    }
    if (typeof data != 'object') {
        cb('data parameter is not an object'); return
    }
    
    var config = Hash.clone(data);
    var directories = config.directories;
    if (typeof directories != 'object') {
        cb('config field "directories" is not an object'); return;
    }
    
    var subDirs = 'config data users'.split(' ');
    
    if (directories.root) {
        subDirs
            .filter(function (subDir) { return !directories[subDir] })
            .forEach(function (subDir) {
                directories[subDir] = directories.root + '/config';
            })
        ;
    }
    
    var missingDirs = subDirs.filter(function (subDir) {
        return !directories[subDir];
    });
    if (missingDirs.length) {
        cb("Directories were not specified and couldn't be "
            + 'inferred with "root": ' + missingDirs.join(', '));
        return;
    }
    
    Step(
        function () {
            // make sure the directories exist
            subDirs.forEach((function (subDir) {
                path.exists(
                    directories[subDir],
                    this.parallel().bind(this, null)
                );
            }).bind(this));
        },
        function (err) {
            if (err) cb(err);
            var exists = Hash.zip(subDirs, [].slice.call(arguments, 1));
            var nex = Hash(exists).filter(function (x) { return !x }).keys;
            if (nex.length) {
                cb('Configuration directories do not exist: ' + nex.join(', '));
            }
            else {
                // deep freeze the result to prevent modifications:
                cb(null, Traverse(config).modify(function (x) {
                    if (typeof x == 'object') {
                        this.update(Object.freeze(x));
                    }
                }));
            }
        }
    );
};
