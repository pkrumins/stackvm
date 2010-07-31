var EventEmitter = require('events').EventEmitter;

exports.Chat = function Chat (params) {
    var self = this;
    var user = params.user;
    var users = params.users;
    
    self.ev = new EventEmitter;
    
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

