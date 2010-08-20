var sys = require('sys');
var DNode = require('dnode');
var Remote = require('../lib/remote');
var User = require('../lib/models/user');

exports['user contacts'] = function (assert) {
    var port = Math.floor(Math.random() * 40000 + 10000);
    
    var server = DNode(function (client, conn) {
        var users = Remote.attach(conn, User.fromList([
            { name : 'biff', contacts : ['eho'] },
            { name : 'eho', contacts : ['biff'] },
        ]));
        
        setTimeout(function () {
           users.biff.emit('_online');
        }, 200);
        
        setTimeout(function () {
           users.eho.emit('_online');
        }, 250);
        
        setTimeout(function () {
           users.biff.emit('_offline');
        }, 200);
        
        return users;
    }).listen(port);
    
    var events = [];
    
    DNode.connect(port, function (remote) {
        remote.biff.subscribe(function (em) {
            em.on('online', function (name) {
                events.push('biff sees ' + name + ' sign in');
            });
            
            em.on('offline', function () {
                events.push('biff offline');
            });
        });
        
        remote.eho.subscribe(function (em) {
            em.on('online', function (name) {
                events.push('eho sees ' + name + ' sign in');
            });
            
            em.on('offline', function () {
                events.push('eho offline');
            });
        });
    });
    
    setTimeout(function () {
        assert.equal(
            events.join(', '),
            'eho sees biff sign in, eho offline, biff sees eho sign in'
        );
        server.end();
    }, 500);
};

