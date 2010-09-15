var Hash = require('traverse/hash');
var qs = require('querystring');
var Store = require('supermarket');

var User = require('./models/user');
var Session = require('./session');

var sessions = Store({
    filename : __dirname + '/../data/sessions.db',
    json : true
});

module.exports = function (db, cb) {
    db.user.all(function (err, names, metas) {
        var users = User.fromHashes(Hash.zip(names, metas));
        
        cb(function (client, conn) {
            var self = this;
            
            self.authenticate = function (f) {
                var cookie = conn.stream.socketio.request.headers.cookie;
                var sid = qs.parse(cookie).connect.sid;
                if (!sid) f('Not authenticated');
                else fromSid(users, conn, sid, f);
            };
            
            // These should signal the ui too:
            conn.on('timeout', reconnect);
            conn.on('end', reconnect);
            
            function reconnect () {
                conn.reconnect(3000, function f (err) {
                    console.log('reconnecting');
                    if (err) { console.log(err); conn.reconnect(1000, f) }
                    console.log('connected');
                });
            }
        });
    });
};

function fromSid (users, conn, sid, cb) {
    sessions.get(sid, function (err, s) {
        if (err) { cb(err); return }
        
        var user = users[s.name];
        if (!Hash(users).has(s.name)) {
            cb('Invalid');
        }
        else {
            var session = new Session(user, conn);
            user.sessions[conn.id] = session;
            
            if (user.connections == 0) user.emit('online');
            user.connections ++;
            
            conn.on('end', function () {
                user.connections --;
                delete user.sessions[conn.id];
                if (user.connections == 0) user.emit('offline');
            });
            
            cb(null, session.attach(conn));
        }
    });
}

