#!/usr/bin/env node
var fs = require('fs');
var sys = require('sys');
var Hash = require('traverse/hash');

var Deployer = require('../lib/setup/deploy');
var Starter = require('../lib/setup/starter');

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
        
        runAction('Deploying StackVM to ' + argv._[1], function (cb) {
            Deployer.deploy(Hash.merge(argv, {
                name : argv._[0],
                base : argv._[1],
                done : cb,
            }));
        });
    },
    undeploy : function () {
        if (argv._.length == 0) {
            throw 'Usage: undeploy [name] {options}';
        }
        
        runAction('Undeploying StackVM at ' + argv._[0], function (cb) {
            Deployer.undeploy(argv._[0], cb);
        });
    },
    start : function () {
        var name = argv._.length ? argv._[0] : 'main';
        runAction('Starting StackVM:' + name, function (cb) {
            Starter.start(name, cb)
        });
    },
    stop : function () {
    },
}[cmd];

function runAction (msg, cb) {
    sys.print(msg + '...    ');
    cb(function (err) {
        if (err) {
            console.log('failed');
            console.error('\n    !!! ' + (err.stack ? err.stack : err) + '\n');
        }
        else console.log('ok')
    });
}

if (action === undefined) {
    console.error('Undefined command ' + sys.inspect(cmd));
}
else {
    try { action() }
    catch (err) {
        console.error('\n' + (err.stack ? err.stack : err));
    }
}
