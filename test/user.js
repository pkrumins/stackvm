var sys = require('sys');
var DNode = require('dnode');
var Remote = require('../lib/remote');
var User = require('../lib/models/user');

exports['user contacts'] = function (assert) {
    var port = Math.floor(Math.random() * 40000 + 10000);
    
    DNode(function (client, conn) {
        return Remote.attach(conn, User.fromList([
            { name : 'biff', contacts : ['eho'] },
            { name : 'eho', contacts : ['biff'] },
        ]));
    }).listen(port);
    
    var events = [];
    
    DNode.connect(port, function (remote) {
        remote.biff.subscribe(function (em) {
            em.on('online', function () {
                events.push('biff online');
            });
            
            em.on('offline', function () {
                events.push('biff offline');
            });
        });
        
        remote.eho.subscribe(function (em) {
            em.on('online', function () {
                events.push('eho online');
            });
            
            em.on('offline', function () {
                events.push('eho offline');
            });
        });
    });
    
    setTimeout(function () {
        console.log(events.join('|'));
    }, 500);
};

