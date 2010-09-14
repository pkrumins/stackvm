#!/usr/bin/env node

var fs = require('fs');
var Store = require('supermarket');
var Hash = require('traverse/hash');

if (process.argv.length <= 2) {
    console.error(
        'Usage: %s %s',
        process.argv.slice(0,2).join(' '),
        '[ json file ]'
    );
    process.exit();
}

var json = JSON.parse(fs.readFileSync(process.argv[2]));

Store(__dirname + '/../data/users.db', function (err, db) {
    Hash(json).forEach(function (user, name) {
        db.set(name, JSON.stringify(user), function (err) {
            if (err) throw err;
        });
    });
});

