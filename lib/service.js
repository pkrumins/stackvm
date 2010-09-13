var DNode = require('dnode');
var Hash = require('traverse/hash');
var User = require('./models/user');
var Session = require('./session');

var qs = require('querystring');

module.exports = function (db, cb) {
    db.user.all(function (err, hashes, metas) {
        var names = metas.map(function (x) { return x.key });
        var users = User.fromHashes(Hash.zip(names, hashes));
        
        cb(function (client, conn) {
            this.authenticate = function () {
                var cookie = conn.stream.socketio.request.headers.cookie;
                var sid = qs.parse(cookie).connect.sid;
                console.log(sid);
            };
        });
    });
};

function Session (users, db, client, conn) {
    if (!(this instanceof Session)) return new Session(db, client, conn);
    
    console.log(conn.stream.socketio.request.headers.cookie);
    
    this.authenticate = function () {};
    /*
    if (params.user && params.key) {
        db.key.get(params.user, function (err, key) {
            if (err) {
                cb(err)
            }
            else if (conn.remoteAddr) {
            }
            else {
            }
        });
    }
    else if (params.user && params.pass) {
        var hash = User.hash(params.pass);
        var name = params.user.toLowerCase();
        var user = users[name];
        
        if (!Hash(users).has(name) || hash != user.hash) {
            cb('Invalid'); return
        };
        
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
    */
};
