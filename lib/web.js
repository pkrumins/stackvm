var path = require('path');
var fs = require('fs');

var player = require('./web/player');
var User = require('./models/user');
var Traverse = require('traverse');
var Hash = require('traverse/hash');

var ejs = require('ejs');
var root = __dirname + '/..';
var views = Hash.map({
    userbar : 'userbar.html'
}, function (base) {
    var file = root + '/views/' + base;
    var body = fs.readFileSync(file, 'utf8');
    fs.watchFile(file, function () {
        try { body = fs.readFileSync(file, 'utf8') }
        catch (_) { }
    });
    return function (vars) {
        return ejs.render(body, { locals : vars });
    };
});

module.exports = function (app, users) {
    function noUser (req) {
        return {
            user : undefined,
            views : views,
            request : req,
            tabs : []
        }
    }
    function locals (req, vars) {
        if (!req.session || !req.session.name) return noUser();
        var user = users[req.session.name];
        return Hash.merge({
            Hash : Hash,
            user : user,
            views : views,
            request : req,
            tabs :
                [].concat(user.admin ? [
                    { path : '/admin', name : 'administer' }
                ] : []),
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
    
    app.get('/ycdemo', function (req, res) {
        req.session.regenerate(function (err) {
            if (err) throw err;
            req.session.name = 'ycdemo';
            res.render('pages/index.html', { locals : locals(req, {}) });
        });
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
    
    app.get('/admin', function (req, res) {
        res.render('pages/admin.html', { locals : locals(req, {
            users : users
        }) });
    });
    
    app.post('/admin/users/delete', function (req, res) {
        res.render('pages/admin.html', { locals : locals(req, {
            users : Hash.filter(users, function (u) {
                return u.name != req.body.name;
            })
        }) });
        console.log('TODO: actually delete users');
    });
    
    app.post('/admin/users/create', function (req, res) {
        var u = {};
        u[req.body.name] = { name : req.body.name };
        res.render('pages/admin.html', { locals : locals(req, {
            users : Hash.merge(users, u)
        }) });
        console.log('TODO: actually create users');
    });
    
    player(app, root);
};
