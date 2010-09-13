var DNode = require('dnode');
var Hash = require('traverse/hash');
var qs = require('querystring');
var nStore = require('nStore');
var sessions = nStore(__dirname + '/../data/sessions.db');

var User = require('./models/user');
var Session = require('./session');

module.exports = function (db, cb) {
    db.user.all(function (err, hashes, metas) {
        var names = metas.map(function (x) { return x.key });
        var users = User.fromHashes(Hash.zip(names, hashes));
        
        cb(function (client, conn) {
            this.authenticate = function (f) {
                var cookie = conn.stream.socketio.request.headers.cookie;
                var sid = qs.parse(cookie).connect.sid;
                if (!sid) f('Not authenticated');
                else fromSid(users, conn, sid, f);
            };
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
