var RemoteEmitter = require('../remote');

module.exports = Mode;
Mode.prototype = new RemoteEmitter;
function Mode (owner, modes) {
    /* Mode semantics vary by service.
        Disks:
            r: copy an image to your account
            w: modify the base image
            x: spawn an instance on the hoster's resources
        Instances:
            r: attach to an instance
            w: keyboard and mouse input
            x: kill the instance
    */
    
    var self = this;
    if (!('users' in modes)) modes.users = {};
    if (!('groups' in modes)) modes.groups = {};
    
    self.subscribe = self.subscribe;
    
    function mask (str, baseMask) {
        return str
            .match(/[+-][rwx]+/g)
            .reduce(
                function (acc,m) {
                    var isPlus = m.slice(0,1) == '+';
                    m.split('').slice(1).forEach(function (letter) {
                        acc[letter] = isPlus;
                    });
                    return acc;
                },
                Object.keys(baseMask).reduce(function (acc, x) {
                    acc[x] = baseMask[x];
                    return acc;
                }, {})
            )
        ;
    }
    
    function most (a, b) {
        return {
            r : a.r || b.r,
            w : a.w || b.w,
            x : a.x || b.x,
        };
    }
    
    function fromString (str) {
        return mask(str, everyone);
    }
    
    self.update = function (params) {
        // concatenate for now, since the parser walks left to right
        if ('users' in params) {
            Object.keys(params.users).forEach(function (name) {
                if (!(name in modes.users)) modes.users[name] = '';
                modes.users[name] += params.users[name];
            });
        }
        if ('groups' in params) {
            Object.keys(params.groups).forEach(function (name) {
                if (!(name in modes.groups)) modes.groups[name] = '';
                modes.groups[name] += params.groups[name];
            });
        }
        self.emit('update', params);
    };
    
    self.forUser = function (user) {
        if (user.name == owner.name) {
            return { r : true, w : true, x : true };
        }
        
        var mode = mask(
            modes.groups.everyone || '', 
            { r : false, w : false, x : false }
        );
        
        if (user.name in modes.users) {
            mode = mask(modes.users[user.name], mode);
        }
        
        // users aggregate permissions from each group they belong to
        owner.groupsFor(user).forEach(function (name) {
            var m = mask(modes.groups[name] || '', mode);
            mode = most(mode, m);
        });
        
        return mode;
    };
}

