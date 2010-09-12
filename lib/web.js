var player = require('./web/player');
var User = require('./models/user');

module.exports = function (app, userDB) {
    var root = __dirname + '/..';
    
    app.configure(function () {
        app.set('views', root + '/views');
        app.register('.html', require('ejs'));
        app.set('view engine', 'html');
    });
    
    app.get('/', function (req, res) {
        var user = { name : req.session.name };
        res.render('index.html', { locals : { user : user } });
    });
    
    app.post('/', function (req, res) {
        userDB.get(req.body.user, function (err, doc, meta) {
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
                    var user = { name : req.session.name };
                    res.render('index.html', { locals : { user : user } });
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
    
    player(app, root);
};
