var player = require('./web/player');

module.exports = function (app) {
    var root = __dirname + '/..';
    
    app.configure(function () {
        app.set('views', root + '/views');
        app.register('.html', require('ejs'));
        app.set('view engine', 'html');
    });
    
    app.get('/', function (req, res) {
        res.render('index.html');
    });
    
    player(app, root);
};
