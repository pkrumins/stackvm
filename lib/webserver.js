// Stackvm webserver.

var sys  = require('sys');
var http = require('http');
var fs   = require('fs');
var url  = require('url');

var allowed_paths = /^\/$|^\/crossdomain.xml$|^(\/screenshots\/|\/screencasts\/)|^(\/js\/|\/css\/|\/img\/)/;
var mime_types = {
    js:   'text/javascript',
    html: 'text/html',
    css:  'text/css',
    jpg:  'image/jpeg',
    gif:  'image/gif',
    png:  'image/png',
    swf:  'application/x-shockware-flash',
    xml:  'application/xml'
};

var dnodeJS = require('dnode/web').source();
var path_handlers = {
    '/':    function(path, req, res) { serve_file('static/html/index.html', res); },
    '/js/dnode.js': function(path, req, res) {
        res.writeHead(200, {'Content-Type': 'text/javascript' });
        res.write(dnodeJS);
        res.end();
    },
    'else': function(path, req, res) { serve_file('static' + path, res); },
}

function ext_to_mime(ext) {
    return mime_types[ext.toLowerCase()] || 'text/plain';
}

function file_to_mime(file) {
    var last_dot = file.lastIndexOf('.');
    if (last_dot >= 0) {
        var ext = file.substring(last_dot+1);
        return ext_to_mime(ext);
    }
    return 'text/plain';
}

function allowed(path) {
    return allowed_paths.test(path);
}

function path_handler(path, req, res) {
    // TODO: make sure /img/../../../../passwd doesn't work
    f = path_handlers[path] || path_handlers['else'];
    f(path, req, res);
}

function serve_file(real_path, res) {
    var path = __dirname + '/../' + real_path;
    fs.stat(path, function(err, stats) {
        if (err) {
            not_found(res, err.message);
            return;
        }
        if (!stats.isFile()) {
            not_found(res, 'not a file');
            return;
        }
        fs.readFile(path, 'binary', function(err, data) {
            if (err) {
                not_found(res, err.message);
                return;
            }
            res.writeHead(200, {'Content-Type': file_to_mime(path) });
            res.write(data, 'binary');
            res.end();
        });
    });
}

function not_found(res, msg) {
    var msg = msg || 'file not found'
    var headers = {'Content-Type': 'text/plain'};

    res.writeHead(404, headers);
    res.write(msg);
    res.end();
}

function conn_handler(req, res)
{
    var parsed = url.parse(req.url);
    if (!allowed(parsed.pathname)) {
        not_found(res, 'forbidden');
        return;
    }
    path_handler(parsed.pathname, req, res);
}

exports.webserver = http.createServer(conn_handler);

