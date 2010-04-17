/*
** Simple stackvm webserver, written in nodejs.
*/

var sys  = require('sys');
var http = require('http');
var fs   = require('fs');
var repl = require('repl');
var url  = require('url');

var bosh_port = 5555;
var bosh_host = 'localhost';

var allowed_paths = /^\/$|^(\/http-bind|\/js\/|\/css\/)/;
var mime_types = {
  js:   'text/javascript',
  html: 'text/html',
  css:  'text/css'
};
var path_handlers = {
  '/':    function(path, req, res) { serve_file('../views/index.html', res); },
  '/http-bind': function(path, req, res) { proxy_bosh(path, req, res); },
  'else': function(path, req, res) { serve_file('../static' + path, res); }
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
  f = path_handlers[path] || path_handlers['else'];
  f(path, req, res);
}

function proxy_bosh(path, req, res) {
  var bosh     = http.createClient(bosh_port, bosh_host);
  var bosh_req = bosh.request(req.method, req.url, req.headers);
  bosh_req.addListener('response', function (bosh_res) {
    res.writeHead(bosh_res.statusCode, bosh_res.headers);
    bosh_res.addListener('data', function (chunk) {
      res.write(chunk);
    });
    bosh_res.addListener('end', function () {
      res.close();
    });
  });
  req.addListener('data', function (chunk) {
    bosh_req.write(chunk);
  });
  req.addListener('end', function () {
    bosh_req.close();
  });
}

function serve_file(real_path, res) {
  fs.stat(real_path, function(err, stats) {
    if (err) {
      not_found(res, err.message);
      return;
    }
    if (!stats.isFile()) {
      not_found(res, 'not a file');
      return;
    }
    fs.readFile(real_path, function(err, data) {
      if (err) {
        not_found(res, err.message);
        return;
      }
      res.writeHead(200);
      res.write(data, {'Content-Type': file_to_mime(real_path) });
      res.close();
    });
  });
}

function not_found(res, msg) {
  var headers = {};
  if (msg) {
    headers = {'Content-Type': 'text/plain'};
  }
  res.writeHead(404, headers);
  if (msg) {
    res.write(msg);
  }
  res.close();
}

function conn_handler(req, res)
{
  var parsed = url.parse(req.url);
  sys.debug('New request: ' + parsed.pathname);
  if (!allowed(parsed.pathname)) {
    not_found(res, 'forbidden');
    return;
  }
  path_handler(parsed.pathname, req, res);
}

http.createServer(conn_handler).listen(9000, '0.0.0.0');
sys.puts('Webserver running at 0.0.0.0:9000');

