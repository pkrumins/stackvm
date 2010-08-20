var EventEmitter = require('events').EventEmitter;
var sys = require('sys');

function RemoteEmitter () {}
module.exports = RemoteEmitter;
RemoteEmitter.RemoteEmitter = RemoteEmitter;

RemoteEmitter.prototype = new EventEmitter;

RemoteEmitter.prototype.subscribe = function (cb) {
    var self = this;
    var conn = self.connection;
    if (!conn) throw 'No connection attached. Use RemoteEmitter.attach()';
    
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
    
    console.log('cb()');
    cb(ev);
    console.log('cb\'d');
};

// Attach a connection to any remote emitters in an object
RemoteEmitter.attach = function (conn, obj) {
    if (typeof obj == 'object' && obj !== null) {
        var copy = { __proto__ : obj.__proto__ };
        Object.keys(obj).forEach(function (key) {
            copy[key] = RemoteEmitter.attach(conn, obj[key]);
        });
        if (obj instanceof RemoteEmitter) {
            Object.defineProperty(copy, 'connection', {
                get : function () { return conn },
                enumerable : false,
            });
        }
        return copy;
    }
    else {
        return obj;
    }
};

