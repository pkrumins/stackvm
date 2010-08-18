var EventEmitter = require('events').EventEmitter;

function RemoteEmitter () {}
module.exports = RemoteEmitter;
RemoteEmitter.RemoteEmitter = RemoteEmitter;

RemoteEmitter.prototype = new EventEmitter;

RemoteEmitter.prototype.subscribe = function (conn, cb) {
    var self = this;
    
    var events = {};
    
    var ev = {
        on : function (name, f) {
            if (!(name in events)) events[name] = [];
            events[name].push(f);
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

