var Hash = require('traverse/hash');

module.exports = function (rules) {
    var self = {};
    
    self.allowed = function (user) {
        var can = Hash(rules).reduce(function (mask, rule, u) {
            return u == user.name ? Hash(mask).merge(rule).items : mask
        }, rules.anonymous || {});
        return Hash(can).length ? can : undefined;
    };
    
    return self;
};
