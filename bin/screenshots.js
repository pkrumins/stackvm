// screenshot service via dnode

var DNode = require('dnode').DNode;
var http = require('http');
var fs = require('fs');
var sys = require('sys');

DNode({
    screenshot : DNode.async(function (png, f) {
        fs.writeFile('moo.png', png, 'binary', function (err) {
            if (err) {
                f({
                    status : 'error',
                    message : err.toString()
                });
                return;
            }
            f({
                status : 'success',
                filename : 'moo.png'
            });
        });
    })
}).listen(9200);

http.createServer(function (req, res) {
    // serve images
}).listen(9201);

sys.log('Screenshot DNode server running on port 9200');
sys.log('Screenshot HTTP serving server running on port 9201');

