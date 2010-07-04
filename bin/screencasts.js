// screencast recording service via dnode

var DNode = require('dnode').DNode;
var http = require('http');
var fs = require('fs');
var sys = require('sys');
var StackedVideo = require('video').StackedVideo;
var Buffer = require('buffer').Buffer;

var screencastDir = "./static/screencasts";

function randomFileName () {
    var fileName = '';
    for (var i = 0; i<32; i++) {
        fileName += String.fromCharCode(Math.random()*26 + 97);
    }
    return fileName;
}

var activeRecordings = {};

DNode({
    startScreencast : DNode.async(function (width, height, f) {
        sys.log('start screencast');
        var fileName = randomFileName() + '.ogv';
        var fullPath = screencastDir + '/' + fileName;
        var video = new StackedVideo(width, height);
        video.setOutputFile(fullPath);
        activeRecordings[fileName] = video;
        f(fileName);
    }),
    stopScreencast : DNode.async(function (fileName, f) {
        sys.log('stop screencast');
        var video = activeRecordings[fileName];
        video.end();
        delete activeRecordings[fileName];
        f({
            status : 'success',
            fileName : fileName
        });
    }),
    newFrame : function (fileName, frame) {
        sys.log('new frame: ' + fileName);
        var video = activeRecordings[fileName];
        var buf = new Buffer(frame.length);
        buf.write(frame, 'binary');
        video.newFrame(buf);
    },
    pushUpdate : function (fileName, frame, x, y, w, h) {
        sys.log('push update');
        var video = activeRecordings[fileName];
        var buf = new Buffer(frame.length);
        buf.write(frame, 'binary');
        video.push(buf, x, y, w, h);
    },
    endPush : function (fileName) {
        sys.log('end push');
        var video = activeRecordings[fileName];
        video.endPush();
    }
}).listen(9300);

http.createServer(function (req, res) {
    if (!/^\/[a-z]{32}\.ogv$/.test(req.url)) {
        res.writeHead(400);
        res.end();
        return;
    }

    var fileName = screencastDir + req.url;
    sys.log(fileName);
    res.writeHead(200, { 'Content-Type': 'video/ogg' });
    // todo: move this to fs.createReadStream (cause the files for longer screencasts are HUGE)
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
}).listen(9301);

sys.log('Screencast DNode server running on port 9300');
sys.log('Screencast HTTP serving server running on port 9301');

