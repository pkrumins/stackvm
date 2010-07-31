#!/usr/bin/env node
// Manage VM instances

var sys = require('sys');
var proc = require('child_process');
var net = require('net');
var DNode = require('dnode');

var sqlite = require('sqlite');
var db = sqlite.openDatabaseSync(__dirname + '/../data/vm.db');

var crypto = require('crypto');

var QemuManager = {
    isAlive : function (row, f) {
        try {
            process.kill(row.pid,0);
            f(true);
        } catch (e) {
            f(false);
        }
    },

    spawn : function (user, vm, procs, f) {
        var port = 5900;
        for (; Object.keys(procs).some(function (uid) {
            return 'localhost:' + port in procs[uid]
        }); port++);

        var hostPort = 'localhost:' + port; // localhost for now

        sys.puts('firing up qemu on ' + hostPort);

        var qemu = proc.spawn('qemu', [
            '-vnc', ':' + (port - 5900),
            __dirname + '/../users/' + user.name + '/vms/' + vm.filename
        ]);

        qemu.on('exit', function () {
            db.query('delete from processes where host = ?', [hostPort]);
            delete procs[user.id][hostPort];
            sys.puts('qemu on ' + hostPort + ' died');
        });
        
        db.query(
            'insert into processes (vm,engine,host,pid) values (?,?,?,?)',
            [ vm.id, 'qemu', hostPort, qemu.pid ],
            function (r) {
                if (r.rowsAffected == 1) {
                    var vmProc = {
                        vm : vm.id,
                        engine : 'qemu',
                        host : hostPort,
                        pid : qemu.pid,
                    };
                    procs[user.id][hostPort] = vmProc;
                    f(vmProc);
                }
                else { f(null) }
            }
        );
    },

    kill : function (vmProc, f) {
        process.kill(vmProc.pid);
        db.query(
            'delete from processes where host = ?', [vmProc.host],
            function (r) { f(r.rowsAffected == 1) }
        );
    },
};

var VMWareManager  = {
    isAlive : function (row, f) {
        var hostPort = row.host.split(':');
        var stream = net.createConnection(hostPort[1], hostPort[0]);
        stream.setTimeout(1000);
        stream.on('connect', function () {
            stream.end();
            f(true);
        });
        stream.on('error', function () {
            f(false);
        });
    },

    spawn : function (user, vm, procs, f) {
        db.query(
            'insert into processes (vm,engine,host,pid) values (?,?,?,?)',
            [ vm.id, 'vmware', vm.host, 'NULL' ],
            function (r) {
                if (r.rowsAffected == 1) {
                    var vmProc = {
                        vm : vm.id,
                        engine : 'vmware',
                        host : vm.host,
                        pid : 0
                    };
                    procs[user.id][vm.host] = vmProc;
                    f(vmProc);
                }
                else { f(null) }
            }
        );
    },

    kill : function (user, host, f) {
        db.query(
            'delete from processes where host = ?', [vmProc.host],
            function (r) { f(r.rowsAffected == 1) }
        );
    },
};

var managers = {
    qemu : QemuManager,
    vmware : VMWareManager
};

db.query('select processes.*, vms.owner from processes, vms '
+ 'where processes.vm = vms.id', function (rows) {
    var procs = {};
    var deadHosts = [];
    rows.forEach(function (row) {
        managers[row.engine].isAlive(row, function (alive) {
            if (alive) {
                if (!(row.owner in procs))
                    procs[row.owner] = {};
                procs[row.owner][row.host] = row;
            }
            else {
                deadHosts.push(row.host);
            }
        });
    });
    
    if (deadHosts.length) {
        var sql = 'delete from processes where '
            + deadHosts.map(function () { return 'host=?' }).join(' or ');
        console.dir(deadHosts);
        console.log(sql);
        db.query(sql, deadHosts, function (r) {
            if (r.rowsAffected != deadHosts.length)
                throw r.rowsAffected + " rows deleted. "
                    + "Should've been " + deadHosts.length;
        });
    }
    
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
    
    self.virtualMachines = function (user, f) {
        db.query(
            'select id, name, engine, filename, host from vms where owner = ? ', [user.id],
            function (rows) {
                var vms = {};
                rows.forEach(function (row) { vms[row.id] = row });
                f(vms);
            }
        );
    };
    
    self.processes = function (user, f) {
        f(procs[user.id] || {});
    };
    
    self.spawn = function (params, f) {
        var user = params.user;
        var vm = params.vm;
        var engine = params.engine; // qemu, vmware, vbox

        if (!(user.id in procs)) procs[user.id] = {};
        managers[engine].spawn(user, vm, procs, f);
    };
    
    self.kill = function (user, host, f) {
        if (host in procs[user.id]) {
            var vmProc = procs[user.id][host]
            managers[vmProc.engine].kill(vmProc, f)
            delete procs[user.id][host];
        }
        else {
            console.log(
                user.id + '/' + host + ' not in procs: '
                + sys.inspect(procs)
            );
        }
    };
    
    self.restart = function (params, f) {
        self.kill(params.user, params.vm.host, function () {
            self.spawn(params, f);
        });
    };
    
    // Authentication will get its own DNode server eventually.
    // For now:
    self.authenticate = function (name, pass, f) {
        var hash = new crypto.Hash('sha512').update(pass).digest('hex');
        db.query(
            'select id, name from users where name = ? and hash = ?', 
            [ name, hash ],
            function (rows) { f(rows[0]) }
        );
    };
}

