var sys = require('sys');
var Mode = require('../lib/models/mode');
var User = require('../lib/models/user');

exports['mode permissions'] = function (assert) {
    var users = User.fromBatch({
        biff : {
            contacts : ['eho'],
            disks : [],
            groups : {}
        },
        eho : {
            contacts : ['biff'],
            disks : [],
            groups : {
                friends : ['biff'],
                enemies : ['feld'],
            },
        },
        feld : {
            contacts : [],
            disks : [],
            groups : {},
        },
    });
    
    assert.equal(
        sys.inspect(users.eho.groupsFor(users.biff)),
        sys.inspect(['friends'])
    );
    
    var modes = {
        biff : new Mode(users.biff, {
            users : { eho : '+w' },
            groups : { everyone : '+r' },
        }),
        eho : new Mode(users.eho, {
            users : { biff : '+r-x' },
            groups : {
                friends : '+w',
                enemies : '-rwx',
                everyone : '-r+x'
            },
        }),
    };
    
    var biffForEho = modes.biff.forUser(users.eho);
    assert.equal(biffForEho.r, true);
    assert.equal(biffForEho.w, true);
    assert.equal(biffForEho.x, false);
    
    var biffForBiff = modes.biff.forUser(users.biff);
    assert.equal(biffForBiff.r, true);
    assert.equal(biffForBiff.w, true);
    assert.equal(biffForBiff.x, true);
    
    var ehoForBiff = modes.eho.forUser(users.biff);
    assert.equal(ehoForBiff.r, true);
    assert.equal(ehoForBiff.w, true);
    assert.equal(ehoForBiff.x, false);
    
    var ehoForFeld = modes.eho.forUser(users.feld);
    assert.equal(ehoForFeld.r, false);
    assert.equal(ehoForFeld.w, false);
    assert.equal(ehoForFeld.x, false);
};

