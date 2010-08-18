#!/usr/bin/env node

var crypto = require('crypto');
var fs = require('fs');

if (process.argv.length < 4) {
    console.log("Usage: useradd.js username password");
    process.exit(1);
}

var user = process.argv[2];
var pass = process.argv[3];
var userDir = __dirname + '/../users/' + user;

try {
    var userDirStat = fs.statSync(userDir);
    if (userDirStat.isDirectory()) {
        console.log("User directory for " + user + " already exists.");
        process.exit(1);
    }
} catch (err) {
    // stat() fails if userDir doesn't exist (which is good, we can add the user)
}

var passHash = new crypto.Hash('sha512').update(pass).digest('hex');

fs.mkdirSync(userDir, 0700);
fs.writeFileSync(userDir + '/passwd', passHash);
fs.mkdirSync(userDir + '/vms', 0700);

console.log("User " + user + " successfully created.");

