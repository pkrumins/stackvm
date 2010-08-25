module.exports = function Mode (owner, modes) {
    var self = this;
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
    
    function mask (str, baseMask) {
        return str
            .match(/[+-][rwx]+/g)
            .reduce(function (acc,m) {
                var isPlus = m.slice(0,1) == '+';
                m.split('').slice(1).forEach(function (letter) {
                    acc[letter] = isPlus;
                });
                return acc;
            }, baseMask)
    }
    
    function most (a, b) {
        return {
            r : a.r || b.r,
            w : a.w || b.w,
            x : a.x || b.x,
        };
    }
    
    var everyone = mask(
        modes.groups.everyone || '', 
        { r : false, w : false, x : false }
    );
    
    function fromString (str) {
        return mask(str, everyone);
    }
    
    self.forUser = function (user) {
        if (user.name == owner.name) {
            return { r : true, w : true, x : true };
        }
        
        var mode = everyone;
        if (user.name in owner.groups) {
            owner.groups[user.name] = {};
        }
        
        // users aggregate permissions from each group they belong to
        owner.groupsFor(user).forEach(function (name) {
            var m = mask(modes.groups[name] || '', mode);
            mode = most(mode, m);
        });
        
        return mode;
    };
}

