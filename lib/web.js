var path = require('path');

var player = require('./web/player');
var User = require('./models/user');
var Traverse = require('traverse');
var Hash = require('traverse/hash');

module.exports = function (app, users) {
    var root = __dirname + '/..';
    
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
        req.session.regenerate(function (err) {
            //if (err) throw err;
            req.session.user = users.ycdemo
            res.render('index.html', {
                locals : { session : req.session }
            });
        });
    });
    
    app.post('/', function (req, res) {
        var user = users[req.body.user];
        if (user && user.hash == User.hash(req.body.pass)) {
            req.session.regenerate(function (err) {
                if (err) throw err;
                req.session.user = {
                    name : user.name,
                    disks : user.disks,
                };
                res.render('index.html', {
                    locals : { session : req.session }
                });
            });
        }
        else {
            res.send('failed');
        }
    });
    
    app.post('/logout', function (req, res) {
        req.session.regenerate(function(err) {
            //if (err) throw err;
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
