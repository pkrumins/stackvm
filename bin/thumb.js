#!/usr/bin/env node
var DNode = require('dnode');
var Hash = require('traverse/hash');

DNode.connect(9090, function (remote, conn) {
    remote.authenticate('robit', 'yc', function (err, account) {
        if (err) { throw err; return }
        
        var simpleLinux = account.disks['linux-0.2.img'];
        simpleLinux.subscribe(function (sub) {
            sub.on('thumb', function (filename) {
                console.log(simpleLinux.filename + '/' + filename);
            });
        });
        simpleLinux.spawn('qemu');
    });
});
