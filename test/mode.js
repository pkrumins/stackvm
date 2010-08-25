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
            groups : {},
        },
    });
    
    var m1 = new Mode(users.biff, {
        users : { biff : '+w' },
        groups : { everyone : '+r' },
    });
};

