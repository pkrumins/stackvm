function Workspace (params) {
    var self = this;
    
    var root = params.root;
    
    /*
    self.spawn = function (disk, engine) {
        account.spawn(disk, engine);
    };
    
    self.attach = function (params) {
        var disk = params.disk;
        var addr = params.addr;
        
        account.attach(addr, function (desktop) {
            if (!desktop) {
                console.log('desktop == null');
                return;
            }
            
            var win = new Window({
                fb : new FB({ desktop : desktop }),
                name : vm.name,
                addr : addr
            });
            
            win.on('minimize', function () {
                account.detach(host);
                quickBar.push(vm, host);
            });
            
            win.on('fullscreen', function () {
                // ...
            });
            
            win.on('close', function () {
                account.detach(host);
            });
            
            win.on('kill', function () {
                account.kill(host);
            });
            
            win.on('restart', function () {
                account.restart(host);
            });
            
            windowPane.append(win.element);
        });
    };
    
    var sideBar = new SideBar({
        user : account.user,
        engines : ['qemu']
    });
    
    sideBar.on('spawn', function (params) { self.spawn(params) });
    sideBar.on('attach', function (params) { self.attach(params) });
    
    root.append( sideBar.element.hide() );
    
    var windowPane = $('<div>')
        .attr('id','window-pane')
        .hide()
        .fadeIn(400)
    ;
    root.append(windowPane);
    
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

