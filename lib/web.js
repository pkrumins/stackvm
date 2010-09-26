var path = require('path');

var player = require('./web/player');
var User = require('./models/user');
var Traverse = require('traverse');
var Hash = require('traverse/hash');

module.exports = function (app, db) {
    var root = __dirname + '/..';
    
    function fromName (name, cb) {
        db.user.get(name, function (err, user) {
            if (err) {
                var dne = 'Document does not exist for ' + req.body.user;
                cb(err.message == dne ? 'No such user' : err);
            }
            else {
                cb(null, Hash.merge({ name : name }, user));
            }
        });
    }
    
    app.configure(function () {
        app.set('views', root + '/views');
        app.register('.html', require('ejs'));
        app.set('view engine', 'html');
    });
    
    app.get('/', function (req, res) {
        res.render('index.html', {
            locals : { session : req.session }
        });
    });
    
    app.get('/ycdemo', function (req, res) {
        fromName('ycdemo', function (err, user) {
            if (err) throw err;
            req.session.regenerate(function (err) {
                req.session.user = user;
                res.render('index.html', {
                    locals : { session : req.session }
                });
            });
        });
    });
    
    app.post('/', function (req, res) {
        fromName(req.body.user, function (err, user) {
            if (err) {
                res.send(err);
            }
            else if (user.hash == User.hash(req.body.pass)) {
                req.session.regenerate(function (err) {
                    if (err) throw err;
                    req.session.user = user;
                    res.render('index.html', {
                        locals : { session : req.session }
                    });
                });
            }
            else {
                res.send('failed');
            }
        });
    });
    
    app.post('/logout', function (req, res) {
        req.session.regenerate(function(err) {
            if (err) throw err;
            res.render('index.html', {
                locals : { session : {} }
            });
        });
    });
    
    app.get(/^\/disks\/([^\/]+)\/([^\/]+)\/thumbnail/, function (req, res) {
        var disk = req.params[0];
        var addr = req.params[1];
        var defFile = root + '/static/img/default-vm.png';
        
        if (!req.session.user || disk.match(/\.\./) || addr.match(/\.\./)) {
            res.sendfile(defFile);
        }
        else {
            var file = root + '/users/' + req.session.user.name
                + '/thumbs/' + disk + '/' + addr + '.png';
            path.exists(file, function (exists) {
                res.sendfile(exists ? file : defFile);
            });
        }
    });
    
    player(app, root);
};
