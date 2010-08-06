Workspace.prototype = new EventEmitter;
function Workspace (params) {
    if (!(this instanceof Workspace)) return new Workspace(params);
    var self = this;
    
    self.element = $('<div>')
        .attr('id','workspace')
    ;
    
    self.attachWindow = function (win) {
        win.on('minimize', function () {
            // ..
        });
        
        win.on('fullscreen', function () {
            // ..
        });
        
        win.on('close', function () {
            // ..
            self.detachWindow(win);
        });
        
        win.on('kill', function () {
            // ..
            self.detachWindow(win);
        });
        
        win.on('restart', function () {
            // ..
        });
        
        self.element.append(win.element);
    };
    
    self.detachWindow = function (win) {
        win.removeAllListeners();
        win.element.remove();
    };
}

