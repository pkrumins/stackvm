#!/usr/bin/env node
// Manage VM instances

var proc = require('child_process');
var DNode = require('dnode');

var sqlite = require('sqlite');
var db = sqlite.openDatabaseSync(__dirname + '/../data/vm.db');

db.query('select port from vms', function (rows) {
    var ports = rows
        .filter(function (row) {
            if (!row.port) return false;
            var alive = false;
            try {
                process.kill(row.pid,0);
                alive = true;
            } catch (e) { }
            return alive;
        })
        .map(function (row) { return row.port })
    ;
    DNode(function (client,conn) {
        return new Manager({
            client : client,
            connection : conn,
            ports : ports,
        });
    }).listen(9077);
});

var sys = require('sys');

function Manager(params) {
    var self = this;
    var client = params.client;
    var conn = params.connection;
    var ports = params.ports;
    
    self.vmList = function (userId, f) {
        db.query(
            'select * from vms where owner = ?', [userId],
            function (rows) { f([].slice.apply(rows)) }
        );
    };
    
    self.spawn = function (params, f) {
        var engine = params.engine; // qemu, vmware, vbox
        var user = params.user;
        var vmId = params.id;
        var filename = params.filename;
        
        var port = 5900;
        for (; ports.indexOf(port) >= 0; port++);
        sys.puts('firing up ' + engine + ' on port ' + port);
        
        if (engine == 'qemu') {
            var qemu = proc.spawn('qemu', [
                '-vnc', ':' + (port - 5900),
                __dirname + '/../users/' + user + '/vms/' + filename
            ]);
            
            qemu.on('exit', function () {
                db.query('update vms set pid = ? where id = ?', [null,vmId]);
                var i = ports.indexOf(port);
                if (i >= 0) ports.splice(i,1);
                sys.puts('qemu on port ' + port + ' died');
            });
            
            db.query(
                'update vms set port = ?, pid = ?, engine = ? where id = ?',
                [ port, qemu.pid, engine, vmId ],
                function (r) {
                    if (r.rowsAffected == 1) {
                        ports.push(port);
                        f(port);
                    }
                    else { f(null) }
                }
            );
        }
        else { f(null) }
    };
    
    self.kill = function (params, f) {
    };
    
    self.restart = function (params, f) {
        self.kill(params, function () {
            self.spawn(params, f);
        });
    };
    
    self.suspend = function (params) {
        // ...
    };
}
