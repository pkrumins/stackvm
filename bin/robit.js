#!/usr/bin/env node
var DNode = require('dnode');
DNode.connect(9090, function (remote) {
    remote.authenticate('robit', 'yc', function (err, account) {
        if (err) { throw err; return }
        
        var ycdemo = account.contacts.ycdemo;
        
        ycdemo.subscribe(function (sub) {
            sub.on('online', function () {
                console.log('ycdemo is online');
                setTimeout(function () {
                    ycdemo.message('Hello friend! I am the stackvM robot.');
                }, 3000);
            });
        });
    });
});
