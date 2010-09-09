#!/usr/bin/env node

var fs = require('fs');
var nStore = require('nStore');
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

var users = nStore(__dirname + '/../data/users.db');

Hash(json).forEach(function (user, name) {
    users.save(name, user, function (err) {
        if (err) throw err;
    });
});

