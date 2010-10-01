var path = require('path');
var fs = require('fs');

var player = require('./web/player');
var User = require('./models/user');
var Traverse = require('traverse');
var Hash = require('traverse/hash');

var ejs = require('ejs');
var root = __dirname + '/..';
var views = Hash.map({
    login_bar : 'login_bar.html'
}, function (file) {
    var body = fs.readFileSync(root + '/views/' + file, 'utf8');
    return function (vars) {
        return ejs.render(body, { locals : vars });
    };
});

module.exports = function (app, users) {
    function locals (req, vars) {
        return Hash.merge({
            Hash : Hash,
            user : users[req.session.name],
            views : views,
        }, vars);
    }
    
    app.configure(function () {
        app.set('views', root + '/views');
        app.register('.html', require('ejs'));
        app.set('view engine', 'html');
    });
    
    app.get('/', function (req, res) {
        res.render('pages/index.html', { locals : locals(req, {}) });
    });
    
    app.get('/ycdemo', function (req, res) {
        req.session.regenerate(function (err) {
            if (err) throw err;
            req.session.name = 'ycdemo';
            res.render('pages/index.html', { locals : locals(req, {}) });
        });
    });
    
    app.post('/', function (req, res) {
        var user = users[req.body.user];
        if (user && user.hash == User.hash(req.body.pass)) {
            req.session.regenerate(function (err) {
                if (err) throw err;
                req.session.name = user.name;
                res.render('pages/index.html', { locals : locals(req, {}) });
            });
        }
        else {
            res.send('failed');
        }
    });
    
    app.post('/logout', function (req, res) {
        req.session.regenerate(function(err) {
            if (err) throw err;
            res.render('pages/index.html', { locals : locals(req, {}) });
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
