Workspace.prototype = new EventEmitter;
function Workspace (params) {
    if (!(this instanceof Workspace)) return new Workspace(params);
    var self = this;
    
    self.element = $('<div>')
        .attr('id','workspace')
    ;
    
    self.attachWindow = function (win) {
        win.titleBar.on('minimize', function () {
            // ..
        });
        
        win.titleBar.on('fullscreen', function () {
            // ..
        });
        
        win.titleBar.on('close', function () {
            self.detachWindow(win);
        });
        
        win.titleBar.on('kill', function () {
            self.detachWindow(win);
        });
        
        win.titleBar.on('restart', function () {
            // ..
        });
        
        self.element.append(win.element);
    };
    
    self.detachWindow = function (win) {
        win.removeAllListeners();
        win.element.remove();
    };
}

