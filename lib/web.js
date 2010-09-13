var player = require('./web/player');
var User = require('./models/user');

module.exports = function (app, db) {
    var root = __dirname + '/..';
    
    app.configure(function () {
        app.set('views', root + '/views');
        app.register('.html', require('ejs'));
        app.set('view engine', 'html');
    });
    
    app.get('/', function (req, res) {
        res.render('index.html', { locals : { session : {
            name : req.session.name
        } } });
    });
    
    app.post('/', function (req, res) {
        db.user.get(req.body.user, function (err, doc, meta) {
            if (err) {
                var dne = 'Document does not exist for ' + req.body.user;
                if (err.message == dne) {
                    res.send('No such user');
                }
                else throw err;
            }
            else if (doc.hash == User.hash(req.body.pass)) {
                req.session.regenerate(function (err) {
                    if (err) throw err;
                    req.session.name = req.body.user;
                    res.render('index.html', { locals : {
                        session : { name : req.session.name }
                    } });
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
            res.render('index.html', { locals : { user : {} } });
        });
    });
    
    app.post('/authorize', function (req, res) {
        if (!req.session.name) { res.send(401); return }
        
        var secret = '';
        for (var i = 0; i < 16; i++)
            secret += '01234567989abcdef'.charAt(Math.random() * 16);
        
        db.key.save(req.session.name, {
            secret : secret,
            remoteAddress : req.socket.remoteAddress,
            expires : Date.now() + 1000 * 10,
        });
        res.send(secret, { 'Content-Type' : 'text/plain' });
    });
    
    player(app, root);
};
