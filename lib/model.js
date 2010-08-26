var RemoteEmitter = require('./remote');

module.exports = function Model (Constructor) {
    Constructor.prototype = new RemoteEmitter;
    
    function NewCons () {
        var self = Object.create(
            new RemoteEmitter,
            { constructor : { value : Constructor } }
        );
        var args = [].slice.call(arguments);
        var r = Constructor.apply(self, args);
        var instance = r === undefined ? self : r;
        instance.subscribe = instance.subscribe;
        return instance;
    };
    
    NewCons.load = function (hash, vars) {
        var acc = {};
        
        Object.keys(hash).forEach(function (key) {
            hash[key].key = key;
            acc[key] = new NewCons(hash[key], vars);
        });
        
        Object.keys(acc).forEach(function (key) {
            if ('finalize' in acc[key]) acc[key].finalize();
        });
        
        return acc;
    };
    
    return NewCons;
};

