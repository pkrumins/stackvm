var EventEmitter = require('events').EventEmitter;
var sys = require('sys');

function RemoteEmitter () {}
module.exports = RemoteEmitter;
RemoteEmitter.RemoteEmitter = RemoteEmitter;

RemoteEmitter.prototype = new EventEmitter;

RemoteEmitter.prototype.withConnection = function (conn, cb) {
    var self = this;
    var events = {};
    
    var ev = {
        on : function (name, f) {
            if (!(name in events)) events[name] = [];
            events[name].push(f);
            // note: might need to check if the connection is still alive inside
            // the callback
            self.on(name, f);
        },
        off : function (name, f) {
            var i = events[name].indexOf(f);
            if (i >= 0) events[name].slice(i,1);
            self.removeListener(name, f);
        }
    };
    
    conn.on('end', function () {
        Object.keys(events).forEach(function (name) {
            events[name].forEach(function (f) {
                ev.off(name, f);
            });
        });
    });
    
    cb(ev);
};

RemoteEmitter.prototype.subscribe = function () {
    throw 'RemoteEmitter not attached before subscribe() called';
};

// Attach a connection to any remote emitters in an object
RemoteEmitter.attach = function (conn, obj) {
    if (typeof obj == 'object' && obj !== null) {
        var copy = { __proto__ : obj.__proto__ };
        Object.keys(obj).forEach(function (key) {
            copy[key] = RemoteEmitter.attach(conn, obj[key]);
        });
        if (obj instanceof RemoteEmitter) {
            if (!obj.connections) obj.connections = 0;
            obj.connections ++;
            
            if (obj.connections == 1) obj.emit('_online');
            
            conn.on('end', function () {
                obj.connections --;
                if (obj.connections == 0) obj.emit('_offline');
            });
            
            copy.subscribe = function (cb) {
                obj.withConnection(conn, cb);
            }
        }
        return copy;
    }
    else {
        return obj;
    }
};

