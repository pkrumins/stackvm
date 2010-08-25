var sys = require('sys');
var DNode = require('dnode');
var Remote = require('../lib/remote');
var User = require('../lib/models/user');

exports['user contacts'] = function (assert) {
    var port = Math.floor(Math.random() * 40000 + 10000);
    
    var server = DNode(function (client, conn) {
        var users = Remote.attach(conn, User.fromBatch({
            biff : { contacts : ['eho'], disks : [] },
            eho : { contacts : ['biff'], disks : [] },
        }));
        
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
    
    var events = 0;
    
    DNode.connect(port, function (remote) {
        remote.biff.subscribe(function (em) {
            em.on('online', function (name) {
                assert.equal(name, 'eho');
                remote.biff.contacts(function (contacts) {
                    contacts.eho.message('sup');
                });
                events ++;
            });
            
            em.on('offline', function () {
                throw 'biff sees somebody sign off';
            });
            
            remote.biff.contacts(function (contacts) {
                em.on('message', function f (msg) {
                    assert.equal(msg.from.name, 'eho');
                    assert.equal(msg.text, 'nada');
                    events ++;
                });
            });
        });
        
        remote.eho.subscribe(function (em) {
            em.on('online', function (name) {
                assert.equal(name, 'biff');
                events ++;
            });
            
            em.on('offline', function (name) {
                assert.equal(name, 'biff');
                events ++;
            });
            
            remote.eho.contacts(function (contacts) {
                em.on('message', function (msg) {
                    assert.equal(msg.from.name, 'biff');
                    assert.equal(msg.text, 'sup');
                    contacts.biff.message('nada');
                    events ++;
                });
            });
        });
    });
    
    setTimeout(function () {
        assert.equal(events, 5);
        server.end();
    }, 500);
};

