var path = require('path');

var player = require('./web/player');
var User = require('./models/user');
var Traverse = require('traverse');
var Hash = require('traverse/hash');

var root = __dirname + '/..';

module.exports = function (app, users) {
    function render (file, vars, req, res) {
        res.render(file, {
            locals : Hash.merge({
                Hash : Hash,
                user : users[req.session.name],
            }, vars)
        });
    }
    
    app.configure(function () {
        app.set('views', root + '/views');
        app.register('.html', require('ejs'));
        app.set('view engine', 'html');
    });
    
    app.get('/', render.bind({}, 'index.html', {}));
    
    app.get('/ycdemo', function (req, res) {
        req.session.regenerate(function (err) {
            if (err) throw err;
            req.session.name = 'ycdemo';
            render('index.html', {}, req, res);
        });
    });
    
    app.post('/', function (req, res) {
        var user = users[req.body.user];
        if (user && user.hash == User.hash(req.body.pass)) {
            req.session.regenerate(function (err) {
                if (err) throw err;
                req.session.name = user.name;
                render('index.html', {}, req, res);
            });
        }
        else {
            res.send('failed');
        }
    });
    
    app.post('/logout', function (req, res) {
        req.session.regenerate(function(err) {
            if (err) throw err;
            render('index.html', {}, req, res);
        });
    });
    
    app.get(/^\/disks\/([^\/]+)\/([^\/]+)\/thumbnail/, function (req, res) {
        var disk = req.params[0];
        var addr = req.params[1];
        var defFile = root + '/static/img/default-vm.png';
        
        if (!req.session.name || disk.match(/\.\./) || addr.match(/\.\./)) {
            res.sendfile(defFile);
        }
        else {
            var file = root + '/users/' + req.session.name
                + '/thumbs/' + disk + '/' + addr + '.png';
            path.exists(file, function (exists) {
                res.sendfile(exists ? file : defFile);
            });
        }
    });
    
    player(app, root);
};
