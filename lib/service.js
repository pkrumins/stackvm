var DNode = require('dnode');
var Hash = require('traverse/hash');
var User = require('./models/user');
var Session = require('./session');

module.exports = function (users) {
    return function (client, conn) {
        return Service.bind(this)(users, client, conn);
    }
};

function Service (users, client, conn) {
    this.authenticate = function (params, cb) {
        if (params.user && params.pass) {
            var hash = User.hash(params.pass);
            var name = params.user.toLowerCase();
            var user = users[name];
            
            if (Hash(users).has(name) && hash == user.hash) {
                var session = new Session(user, conn);
                user.sessions[conn.id] = session;
                
                if (user.connections == 0) user.emit('online');
                user.connections ++;
                
                conn.on('end', function () {
                    user.connections --;
                    delete user.sessions[conn.id];
                    if (user.connections == 0) user.emit('offline');
                });
                
                cb(session.attach(conn));
            }
            else {
                cb(null);
            }
        }
    };
}
