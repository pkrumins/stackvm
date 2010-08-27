var fs = require('fs');

var jsFiles = [
    __dirname + '/../static/js/vendor/jquery.js',
    __dirname + '/../static/js/vendor/jquery-ui.js',
    __dirname + '/../static/js/vendor/jquery.mousewheel.js',
    __dirname + '/../static/js/vendor/jquery.cookie.js',
    __dirname + '/../static/js/vendor/jquery.js',
    __dirname + '/../static/js/util/events.js',
    __dirname + '/../static/js/util/keymap.js',
    __dirname + '/../static/js/session.js',
    __dirname + '/../static/js/ui.js',
    __dirname + '/../static/js/ui/workspace.js',
    __dirname + '/../static/js/ui/fb/display.js',
    __dirname + '/../static/js/ui/fb.js',
    __dirname + '/../static/js/ui/chat.js',
    __dirname + '/../static/js/ui/window.js',
    __dirname + '/../static/js/ui/window/titlebar.js',
    __dirname + '/../static/js/ui/sidebar.js',
    __dirname + '/../static/js/ui/sidebar/menustack.js',
    __dirname + '/../static/js/ui/taskbar.js',
];

var cssFiles = [
    __dirname + '/../static/css/login.css',
    __dirname + '/../static/css/workspace.css',
    __dirname + '/../static/css/fb.css',
    __dirname + '/../static/css/window.css',
    __dirname + '/../static/css/sidebar.css',
    __dirname + '/../static/css/chat.css',
];

var cache = {};

function buildCache(type) {
    var files = type == 'js' ? jsFiles : cssFiles;
    cache[type] = '';
    files.forEach(function (file) {
        cache[type] += fs.readFileSync(file).toString();
    });
}

[
    { what : jsFiles, type : 'js' },
    { what : cssFiles, type : 'css' }
].forEach(function (item) {
    item.what.forEach(function (file) {
        fs.watchFile(file, function () {
            console.log('file ' + file + ' changed, updating ' + item.type + ' cache.');
            setTimeout(function () {
                buildCache(item.type);
            }, 100); // TODO: samba issues
        });
    });
    buildCache(item.type);
});

module.exports = {
    js : function () { return cache['js']; },
    css : function () { return cache['css']; }
};

