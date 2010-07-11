// screenshot service via dnode

var DNode = require('dnode').DNode;
var http = require('http');
var fs = require('fs');
var sys = require('sys');

var screenshotDir = "./static/screenshots";

function randomFileName () {
    var fileName = '';
    for (var i = 0; i<32; i++) {
        fileName += String.fromCharCode(Math.random()*26 + 97);
    }
    return fileName;
}

DNode({
    screenshot : function (png, f) {
        var fileName = randomFileName() + '.png';
        var fullPath = screenshotDir + '/' + fileName;
        fs.writeFile(fullPath, png, 'binary', function (err) {
            if (err) {
                f({
                    status : 'error',
                    message : err.toString()
                });
                return;
            }
            f({
                status : 'success',
                fileName : fileName
            });
        });
    }
}).listen(9200);

http.createServer(function (req, res) {
    if (!/^\/[a-z]{32}\.png$/.test(req.url)) {
        res.writeHead(400);
        res.end();
        return;
    }

    var fileName = screenshotDir + req.url;
    sys.log(fileName);
    res.writeHead(200, { 'Content-Type': 'image/png' });
    fs.readFile(fileName, 'binary', function (err, data) {
        if (err) {
            var msg = "Error reading " + fileName + ": " + err.toString();
            sys.log(msg);
            res.write(msg);
            res.end();
            return;
        }
        res.write(data, 'binary');
        res.end();
    });
}).listen(9201);

sys.log('Screenshot DNode server running on port 9200');
sys.log('Screenshot HTTP serving server running on port 9201');

