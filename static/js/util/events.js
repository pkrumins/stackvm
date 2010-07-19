// node.js-style EventEmitters for client-side javascript
/* Usage:
    require('events.js');
    Constructor.prototype = new EventEmitter;
    function Constructor () { ... }
*/

function EventEmitter () {
    var self = this;
    
    var listeners = {};
    this.listeners = function (name) {
        if (!listeners[name]) listeners[name] = [];
        return listeners[name];
    };
    
    this.emit = function (name) {
        var args = [].concat.apply([],arguments).slice(1);
        this.listeners(name).forEach(function (f) {
            try {
                f.apply(self,args);
            }
            catch (error) {
                if (self.listeners('error')) {
                    self.emit('error', error);
                }
                else {
                    throw error;
                }
            }
        });
        return this;
    };
    
    this.addListener = function (name, listener) {
        self.emit('newListener', name, listener);
        this.listeners(name).push(listener);
        return this;
    };
    
    this.removeListener = function (name, listener) {
        var i = this.listeners(name).find(listener);
        if (i >= 0) this.listeners(name).splice(i,1);
        return this;
    };
    
    this.removeAllListeners = function (name) {
        this.listeners(name) = [];
        return this;
    };
};

