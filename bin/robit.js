#!/usr/bin/env node
var DNode = require('dnode');
var Hash = require('traverse/hash');

DNode.connect(9090, function (remote) {
    remote.authenticate('robit', 'yc', function (err, account) {
        if (err) { throw err; return }
        
        var ycdemo = account.contacts.ycdemo;
        
        var simpleLinux = account.disks['linux-0.2.img'];
        simpleLinux.subscribe(function (sub) {
            sub.on('spawn', function (proc) {
                ycdemo.subscribe(function (sub) {
                    sub.on('online', script.bind({}, ycdemo, proc));
                });
            });
        });
        simpleLinux.spawn('qemu');
    });
});

function script (yc, proc) {
    console.log('ycdemo is online');
    
    var items = [
        [ 2000, function () {
            yc.message('Hello! I am the StackVM robot.');
        } ],
        [ 2000, function () {
            yc.message("I'm here to show off some nifty "
                + 'features, like this chat!');
        } ],
        [ 2000, function () {
            yc.message("Or here, I'll share a VM with you.");
        } ],
        [ 1500, function () {
            yc.share('process', proc.address, {
                ycdemo : { input : true, view : true }
            });
        } ],
        [ 10000, function () {
            yc.message('The VM is bound to the framebuffer, '
                + 'so you can `xinit` and everything!');
        } ],
        [ 20000, function () {
            yc.message('Now try spawning your own VM. '
                + 'Click "qemu" under "disk images" in the left pane.');
        } ],
    ];
    
    (function next (yc) {
        var ix = items.shift();
        if (ix) setTimeout(function () { ix[1](); next() }, ix[0]);
    })();
}
