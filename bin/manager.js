#!/usr/bin/env node
// Manage VM instances

var sys = require('sys');
var proc = require('child_process');
var DNode = require('dnode');

var sqlite = require('sqlite');
var db = sqlite.openDatabaseSync(__dirname + '/../data/vm.db');

db.query('select processes.*, vms.owner from processes, vms '
+ 'where processes.vm = vms.id', function (rows) {
    var procs = {};
    rows.forEach(function (row) {
        var alive = false;
        try {
            process.kill(row.pid,0);
            alive = true;
        } catch (e) {
            db.query('delete from processes where pid = ?', [row.pid]);
        }
        if (alive) {
            if (!(row.owner in procs))
                procs[row.owner] = {};
            procs[row.owner][row.port] = row;
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

function Manager(params) {
    var self = this;
    var client = params.client;
    var conn = params.connection;
    var procs = params.processes;
    
    self.vmList = function (user, f) {
        db.query(
            'select id, name, filename from vms where owner = ? ', [user.id],
            function (rows) { f([].slice.apply(rows)) }
        );
    };
    
    self.processList = function (user, f) {
        f(procs[user.id]);
    };
    
    self.spawn = function (params, f) {
        var user = params.user;
        var vm = params.vm;
        var engine = params.engine; // qemu, vmware, vbox
        
        if (!(user.id in procs)) procs[user.id] = {};
        
        var port = 5900;
        for (; Object.keys(procs).some(function (uid) {
            return port in procs[uid]
        }); port++);
        
        sys.puts('firing up ' + engine + ' on port ' + port);
        
        if (engine == 'qemu') {
            var qemu = proc.spawn('qemu', [
                '-vnc', ':' + (port - 5900),
                __dirname + '/../users/' + user.name + '/vms/' + vm.filename
            ]);
            
            qemu.on('exit', function () {
                db.query('delete from processes where port = ?', [port]);
                delete procs[user.id][port];
                sys.puts('qemu on port ' + port + ' died');
            });
            
            db.query(
                'insert into processes (vm,engine,port,pid) values (?,?,?,?)',
                [ vm.id, engine, port, qemu.pid ],
                function (r) {
                    if (r.rowsAffected == 1) {
                        procs[user.id][port] = {
                            vm : vm.id,
                            engine : engine,
                            port : port,
                            pid : qemu.pid,
                            owner : user.id,
                        };
                        f(port);
                    }
                    else { f(null) }
                }
            );
        }
        else { f(null) }
    };
    
    self.kill = function (user, port, f) {
        process.kill(procs[user.id][port].pid);
        delete procs[user.id][port];
        db.query('delete from processes where port = ?', [port], f);
    };
    
    self.restart = function (params, f) {
        self.kill(params.user, params.vm.port, function () {
            self.spawn(params, f);
        });
    };
}
