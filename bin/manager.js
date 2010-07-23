#!/usr/bin/env node
// Manage VM instances

var proc = require('child_process');
var DNode = require('dnode');

var sqlite = require('sqlite');
var db = sqlite.openDatabaseSync(__dirname + '/../data/vm.db');

db.query('select * from processes', function (rows) {
    var procs = {};
    rows.forEach(function (row) {
        try {
            process.kill(row.pid,0);
            if (!(row.vm in procs)) procs[row.vm] = {};
            procs[row.vm][row.port] = row;
        } catch (e) {
            db.query('delete from processes where pid = ?', [row.pid]);
        }
    });
    
    DNode(function (client,conn) {
        return new Manager({
            client : client,
            connection : conn,
            processes : procs,
        });
    }).listen(9077);
});

var sys = require('sys');

function Manager(params) {
    var self = this;
    var client = params.client;
    var conn = params.connection;
    var procs = params.processes;
    
    self.vms = function (uId, f) {
        db.query(
            'select id, name, filename from vms where owner = ? ', [uId],
            function (rows) { f([].slice.apply(rows)) }
        );
    };
    
    self.processes = function (vmId, f) {
        f(processes[vmId]);
    };
    
    self.spawn = function (params, f) {
        var engine = params.engine; // qemu, vmware, vbox
        var user = params.user;
        var vmId = params.id;
        var filename = params.filename;
        
        var port = 5900;
        for (; Object.keys(procs).some(function (key) {
            return port in procs[key]
        }); port++);
        
        sys.puts('firing up ' + engine + ' on port ' + port);
        
        if (engine == 'qemu') {
            var qemu = proc.spawn('qemu', [
                '-vnc', ':' + (port - 5900),
                __dirname + '/../users/' + user + '/vms/' + filename
            ]);
            
            qemu.on('exit', function () {
                db.query('delete from processes where port = ?', [port]);
                delete procs[vmId][port];
                sys.puts('qemu on port ' + port + ' died');
            });
            
            db.query(
                'insert into processes (vm,engine,port,pid) values (?,?,?,?)',
                [ vmId, engine, port, qemu.pid ],
                function (r) {
                    if (r.rowsAffected == 1) {
                        if (!(vmId in procs)) procs[vmId] = {};
                        procs[vmId][port] = {
                            vm : vmId,
                            engine : engine,
                            port : port,
                            pid : qemu.pid,
                        };
                        f(port);
                    }
                    else { f(null) }
                }
            );
        }
        else { f(null) }
    };
    
    self.kill = function (vmId, port) {
        sys.puts(sys.inspect(procs));
        process.kill(procs[vmId][port].pid);
        delete procs[vmId][port];
        db.query(
            'delete from processes where vm = ? and port = ?',
            [vmId,port]
        );
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
