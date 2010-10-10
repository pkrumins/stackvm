#!/usr/bin/env node
var fs = require('fs');
var sys = require('sys');
var setup = require('../lib/models/setup');

var argv = require('optimist')
    .usage(fs.readFileSync(__dirname + '/../doc/cli.txt', 'utf8'))
    .check(function (args) { if (args._.length == 0) throw '' })
    .argv;

var cmd = argv._.shift();
var action = {
    deploy : function () {
        if (argv._.length == 0) {
            throw 'Usage: deploy [directory] {options}'
        }
        setup.deploy(argv._[0], argv, function (err) {
            if (err) console.error('\n    !!! ' + err + '\n');
        });
    }
}[cmd];

if (action === undefined) {
    console.error('Undefined command ' + sys.inspect(cmd));
}
else {
    try {
        action();
    }
    catch (err) {
        console.error(err);
    }
}
