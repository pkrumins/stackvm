// node.js-style EventEmitters for client-side javascript

function EventEmitter () {
    if (!(this instanceof EventEmitter)) return new EventEmitter;
}

EventEmitter.prototype.listeners = function (name) {
    if (!this._events) this._events = {};
    if (!(name in this._events)) this._events[name] = [];
    return this._events[name];
};
    
EventEmitter.prototype.emit = function (name) {
    var self = this;
    
    var args = [].slice.call(arguments,1);
    this.listeners(name).forEach(function (f) {
        f.apply(self,args);
    });
    return this;
};

EventEmitter.prototype.addListener = function (name, listener) {
    this.emit('newListener', name, listener);
    this.listeners(name).push(listener);
    return this;
};

EventEmitter.prototype.on = EventEmitter.prototype.addListener;

EventEmitter.prototype.removeListener = function (name, listener) {
        var i = this.listeners(name).find(listener);
        if (i >= 0) this.listeners(name).splice(i,1);
        return this;
    };
    
EventEmitter.prototype.removeAllListeners = function (name) {
    this._events[name] = [];
    return this;
};

