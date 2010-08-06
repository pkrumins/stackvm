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
    
    /*
    var logo = $('<img>')
        .attr('src','/img/stackvm-200x48.png')
        .attr('id','logo')
        .hide()
        .fadeIn(400)
        .toggle(
            function () { sideBar.element.fadeIn(400) },
            function () { sideBar.element.fadeOut(400) }
        )
    ;
    root.append(logo);
    
    var quickBar = new QuickBar;
    root.append(quickBar.element);
    quickBar.on('restore', function (vm, host) {
        self.attach(vm, host);
    });
    
    var sheet = $('<div>')
        .attr('id','sheet')
        .hide()
    ;
    root.append(sheet);
    */
}

