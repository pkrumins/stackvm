#!/usr/bin/env node

var argv = require('optimist').argv;
var spawn = require('child_process').spawn;

// not sure how to proceed


function hasQemu (cb) {
    var which = spawn('which', ['qemu']);
    which.on('exit', function (code) {
        cb(code==0);
    });
}

exports.hasQemu = hasQemu;

