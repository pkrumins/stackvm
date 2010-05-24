// Stackvm mediator.

var sys  = require('sys');
var http = require('http');
var net  = require('net');
var buffer = require('buffer');

// for now just a map, later lookup from database, etc.
var vm_map = {
    linux1: {'host': '127.0.0.1', 'port': 25900},
    dos1:   {'host': '127.0.0.1', 'port': 25901}
};

var vm_connection_map = {};

function handler_start_vm(msg, client) {
    if (vm_connection_map[msg.vm_id]) {
        client.send(JSON.stringify({
            vm_id:  msg.vm_id,
            action: 'connected'
        }));
        stackvm_socket.write('update ' + msg.vm_id + ' 0\n');
        return;
    }

    var vm_host = vm_map[msg.vm_id]['host'];
    var vm_port = vm_map[msg.vm_id]['port'];

    stackvm_socket = net.createConnection(vm_port, vm_host);
    stackvm_socket.setTimeout(0);
    stackvm_socket.setKeepAlive(true, 60*1000);
    stackvm_socket.setEncoding('binary');
    stackvm_socket.addListener('error', function(e) {
        sys.log("VM " + msg.vm_id + " connection error: " + String(e));
        client.send(JSON.stringify({
            vm_id:   msg.vm_id,
            action:  'error',
            message: String(e)
        }));
    });
    stackvm_socket.addListener('connect', function() {
        sys.log("Connected to " + msg.vm_id);
        client.send(JSON.stringify({
            vm_id:  msg.vm_id,
            action: 'connected'
        }));
        stackvm_socket.write('update ' + msg.vm_id + ' 0\n');
    });
    stackvm_socket.addListener('disconnect', function(errno) {
        sys.log("Disconnected from " + msg.vm_id);
        client.send(JSON.stringify({
            vm_id:  msg.vm_id,
            action: 'disconnected'
        }));
    });

    var EXPECT_SIZE = 0;
    var EXPECT_DATA = 1;

    vm_connection_map[msg.vm_id] = {
        'state':  EXPECT_SIZE
    };

    stackvm_socket.addListener('data', function(data) {
        // for now just send the data to the client, multiplex later on client id, and
        // keep track of update id.
        connmap = vm_connection_map[msg.vm_id];
        if (connmap.state == EXPECT_SIZE) {
            var size = parseInt(data);
            connmap.state = EXPECT_DATA;
            connmap.buffer = new buffer.Buffer(size);
            connmap.size = size;
            connmap.written = 0;
        }
        else if (connmap.state == EXPECT_DATA) {
            connmap.written += connmap.buffer.write(data, 'binary', connmap.written);
            if (connmap.written == connmap.size) {
                sys.log(connmap.buffer.toString());
                client.send(connmap.buffer.toString());
                connmap.state = EXPECT_SIZE;
            }
        }
    });
    vm_connection_map[msg.vm_id].socket = stackvm_socket;
}

function handler_key_down(msg, client) {
    vm_connection_map[msg.vm_id].socket.write('key_down ' + msg.vm_id + ' ' + msg.key + '\n');
}

function handler_key_up(msg, client) {
    vm_connection_map[msg.vm_id].socket.write('key_up ' + msg.vm_id + ' ' + msg.key + '\n');
}

function handler_pointer(msg, client) {
    to_write = 'pointer ' + msg.vm_id + ' ' + msg.x + ' ' + msg.y + ' ' + msg.mask + '\n';
    vm_connection_map[msg.vm_id].socket.write(to_write);
}

// TODO: just call handler_*(msg, client) for the given action
var handlers = {
    'start_vm': function(msg, client) { handler_start_vm(msg, client); },
    'redraw_screen': function(msg, client) { handler_redraw_screen(msg, client); },
    'key_down': function(msg, client) { handler_key_down(msg, client); },
    'key_up': function(msg, client) { handler_key_up(msg, client); },
    'pointer': function(msg, client) { handler_pointer(msg, client); },
}

function handle_message(msg, client) {
    if (!msg.vm_id) {
        sys.log("Unknown message:");
        sys.log(JSON.stringify(msg));
        return;
    }
    if (!vm_map[msg.vm_id]) {
        sys.log("Unknown VM ID:");
        sys.log(JSON.stringify(msg));
        client.send(JSON.stringify({
            error: "There is no VM with id '" + msg.vm_id + "'"
        }));
        return;
    }
    if (!msg.action) {
        sys.log("Unknown action:");
        sys.log(JSON.stringify(msg));
        client.send(JSON.stringify({
            error: "No action specified."
        }));
        return;
    }
    handler = handlers[msg.action];
    if (!handler) {
        sys.log("Unknown handler.");
        sys.log(JSON.stringify(msg));
        return;
    }
    handler(msg, client);
}

exports.handle_message = handle_message;

