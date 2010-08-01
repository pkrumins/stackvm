function Workspace (params) {
    var self = this;
    if (!(this instanceof Workspace)) return new Workspace(params);
    
    $('form#login').fadeOut(400);
    
    var root = params.root;
    var account = params.account;
    var instances = new EventEmitter;
    var contacts = new EventEmitter;
    
    self.spawn = function (vm, engine) {
        account.spawn({ vm : vm.id, engine : engine }, function (proc) {
            instances.emit('spawn', vm, proc);
        });
    };
    
    self.attach = function (vm, host) {
        account.attach(host, function (desktop) {
            if (!desktop) {
                console.log('desktop == null');
                return;
            }
            
            var win = new Window({
                fb : new FB({ desktop : desktop }),
                name : vm.name
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
                instances.emit('kill', host);
                account.kill(host);
            });
            
            win.on('restart', function () {
                account.restart(host);
            });
            
            windowPane.append(win.element);
        });
    };
    
    var sideBar = new SideBar({
        instances : instances,
        contacts : contacts,
        engines : ['qemu']
    });
    sideBar.on('spawn', function (vm, engine) {
        self.spawn(vm, engine);
    });
    sideBar.on('attach', function (vm, host) {
        self.attach(vm, host);
    });
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
    
    account.instances(function (list) {
        instances.emit('list', list);
    });
}

