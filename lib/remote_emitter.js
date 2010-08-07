var EventEmitter = require('events').EventEmitter;

module.exports = RemoteEmitter;
function RemoteEmitter (emitter,conn) {
    var events = {};
    var remote = {};
    Object.keys(emitter).forEach(function (key) {
        remote[key] = emitter[key];
    });
    
    remote.on = function (ev, cb) {
        if (!events[ev]) events[ev] = [];
        events[ev].push(cb);
        emitter.on(ev, cb);
    };
    
    remote.emit = function () {
        emitter.emit.apply(emitter, [].slice.call(arguments));
    };
    
    remote.removeListener = function (ev, cb) {
        if (!(ev in events)) events[ev] = [];
        var i = events[ev].indexOf(cb);
        if (i >= 0) events[ev].splice(i,1);
        if (events[ev].length == 0) delete events[ev];
        emitter.removeListener(ev, cb);
    };
    
    conn.on('end', function () {
        Object.keys(events).forEach(function (ev) {
            events[ev].forEach(function (cb) {
                remote.removeListener(ev, cb);
            });
        });
    });
    
    return remote;
};
