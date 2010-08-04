// Push out resource's events to multiple remote clients
var EventEmitter = require('events').EventEmitter;

module.exports = function Resource (resource) {
    if (!(this instanceof Resource)) return new Resource(resource);
    var self = this;
    
    var clients = {};
    
    // patch into events and send them to the remote clients
    resource.emit = function () {
        var args = [].slice.apply(arguments);
        
        // pass them along to local listeners first
        resource.constructor.prototype.emit.apply(resource, args);
        
        // then to the remotes
        Object.keys(clients).forEach(function (id) {
            var em = clients[id].emitter;
            em.emit.apply(em, args);
        });
    };
    
    self.subscribe = function (em, conn) {
        clients[conn.clientId] = {
            emitter : em,
            connection : conn,
        };
        
        conn.on('end', function () {
            delete clients[conn.clientId];
        });
    };
    
    self.unsubscribe = function (conn) {
        delete clients[conn.clientId];
    };
};

