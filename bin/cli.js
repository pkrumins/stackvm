#!/usr/bin/env node
var fs = require('fs');
var sys = require('sys');
var deploy = require('../lib/setup/deploy');
var Hash = require('traverse/hash');

var argv = require('optimist')
    .usage(fs.readFileSync(__dirname + '/../doc/cli.txt', 'utf8'))
    .check(function (args) { if (args._.length == 0) throw '' })
    .argv;

function updateJSON (file, f) {
    fs.readFile(file, function (err, buf) {
        var data = JSON.parse(buf.toString());
        var out = JSON.stringify(f(data));
        fs.write(file, out);
    });
}

var cmd = argv._.shift();
var action = {
    deploy : function () {
        if (argv._.length < 2) {
            throw 'Usage: deploy [name] [directory] {options}';
        }
        
        sys.print('Deploying StackVM to ' + argv._[1] + '...    ');
        deploy(Hash.merge(argv, {
            name : argv._[0],
            base : argv._[1],
            done : function (err) {
                if (err) {
                    console.log('failed');
                    console.error('\n    !!! '
                        + (err.stack ? err.stack : err) + '\n'
                    );
                }
                else console.log('ok');
            },
        }));
    },
    start : function () {
        var name = argv._.length ? argv._[0] : 'main';
    },
    stop : function () {
    },
}[cmd];

if (action === undefined) {
    console.error('Undefined command ' + sys.inspect(cmd));
}
else {
    try { action() }
    catch (err) {
        console.error('\n' + (err.stack ? err.stack : err));
    }
}
