var EventEmitter = require('events').EventEmitter;

exports.Chat = function Chat (params) {
    var self = this;
    
    var ev = new EventEmitter;
    self.on = function (name,f) { ev.on(name,f) };
    
    self.contacts = function (f) {
        f(['foo','bar','baz']);
    };
    
    self.chat = function (who) {
        self.send = function (who, msg) {
        };
        users[who]
    };
};

exports.Conversation = function (params) {
    var self = this;
    
    self.invite = function (who) {
        ///
    };
};

