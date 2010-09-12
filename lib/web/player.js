var fs = require('fs');
var glob = require('glob');
var Hash = require('traverse/hash');

module.exports = function (app, root) {
    var player = {
        stylesheets :
            'chat.css fb.css login.css sidebar.css window.css workspace.css'
            .split(' ')
            .map(function (x) { return '/css/player/' + x })
        ,
        scripts :
            [
                'vendor/jquery.js',
                'vendor/jquery-ui.js',
                'vendor/jquery.mousewheel.js',
                'util/events.js',
                'util/keymap.js',
            ].concat(
                glob.globSync(root + '/static/js/*.js'),
                glob.globSync(root + '/static/js/ui/*.js'),
                glob.globSync(root + '/static/js/ui/*/*.js')
            ).map(function (x) {
                return '/js/' + x.replace(root + '/static/js/', '')
            })
        ,
    };
    
    var bundle = Hash.map(player, function (x) {
        return x.map(function (file) {
            return fs.readFileSync(root + '/static' + file);
        }).join('\n');
    });
    
    app.get('/css/player.css', function (req, res) {
        res.send(bundle.stylesheets, { 'Content-Type' : 'text/css' });
    });
    
    app.get('/js/player.js', function (req, res) {
        res.send(bundle.scripts, { 'Content-Type' : 'text/html' });
    });
    
    app.get('/player/', function (req, res) {
        app.configure('development', function () {
            res.render('player.html', {
                locals : player,
                layout : false,
            });
        });
        
        app.configure('production', function(){
            res.render('player.html', {
                locals : {
                    stylesheets : [ '/css/player.css' ],
                    scripts : [ '/js/player.js' ],
                },
                layout : false,
            });
        });
    });
};
