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
        res.render('index.html', {
            locals : { user : null }
        });
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
                res.send('=^_^=');
            }
            else {
                res.send('failed');
            }
        });
    });
    
    player(app, root);
};
