var DNode = require('dnode');
var Hash = require('traverse/hash');
var User = require('./models/user');
var Session = require('./session');

module.exports = function (db, cb) {
    db.user.all(function (err, hashes, metas) {
        var names = metas.map(function (x) { return x.key });
        var users = User.fromHashes(Hash.zip(names, hashes));
        
        cb(function (client, conn) {
            this.authenticate = function (params, cb) {
                authenticate(Hash.merge(params, {
                    client : client,
                    connection : conn,
                    users : users,
                }), cb);
            };
        });
    });
};

function authenticate (params, cb) {
    var conn = params.connection;
    var users = params.users;
    
    if (params.key) {
        // create session from key.db entry here
    }
    else if (params.user && params.pass) {
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
