var fs = require('fs');
var Step = require('step');

exports.createUser = function (config, name, cb) {
    var root = config.directories.users + '/' + name;
    Step(
        function () {
            if (name in ['.','..',''] || name.match(/\//)) {
                cb('Names cannot contain "/" nor equal "." nor ".." nor ""');
            }
            else {
                fs.mkdir(root, 0700 , this);
            }
        },
        function (err) {
            if (err) cb(err);
            else {
                fs.mkdir(root + '/disks', 0700 , this.parallel());
                fs.mkdir(root + '/thumbs', 0700 , this.parallel());
            }
        },
        function (err) {
            if (err) cb(err);
            else cb(null);
        }
    );
};
