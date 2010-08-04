var EventEmitter = require('events').EventEmitter;
var DNode = require('dnode');

module.exports = Chat;
Chat.prototype = new EventEmitter;
function Chat (params) {
    var self = this;
    var conn = params.connection;
    var user = params.user;
    
    DNode.expose(self, 'on');
    
    self.send = function (uid, msg) {
        Chat[uid].emit('message', user, msg);
    };
    
    Chat[user.id] = self;
    conn.on('end', function () {
        delete Chat[user.id];
    });
};

