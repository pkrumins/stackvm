function Workspace (rootElem, account) {
    if (!(this instanceof Workspace))
        return new Workspace(rootElem, account);
    var self = this;
    
    $('form#login').fadeOut(400);
    
    var leftPane = $('<div>')
        .addClass('left-pane')
        .attr('id','left-pane')
        .hide()
    ;
    rootElem.append(leftPane);
    
    var vmPane = $('<div>').addClass('info-pane');
    leftPane.append(vmPane);
    
    var windowPane = $('<div>')
        .attr('id','window-pane')
        .hide()
        .fadeIn(400)
    ;
    rootElem.append(windowPane);
    
    var logo = $('<img>')
        .attr('src','/img/stackvm-200x48.png')
        .attr('id','logo')
        .hide()
        .fadeIn(400)
        .toggle(
            function () { leftPane.fadeIn(400) },
            function () { leftPane.fadeOut(400) }
        )
    ;
    rootElem.append(logo);
    
    var quickBar = new QuickBar;
    rootElem.append(quickBar.element);
    quickBar.on('restore', function (vm, host) {
        self.attach(vm, host);
    });
    
    var sheet = $('<div>')
        .attr('id','sheet')
        .hide()
    ;
    rootElem.append(sheet);
    
    var infoPanes = {};
    self.addInfoPane = function (vm) {
        var elem = $('<div>')
            .hide()
            .addClass('info-pane')
            .append(
                $('<div>')
                    .addClass('back')
                    .text('back')
                    .click(function () {
                        vmPane.fadeIn(400);
                        elem.fadeOut(400);
                    })
                ,
                $('<p>').text(vm.name),
                $('<p>').text('Instances:'),
                $('<p>').append.apply(
                    $('<p>').addClass('instance-list'),
                    vm.processes.map(function (proc) {
                        return $('<p>').append($('<a>')
                            .text(proc.engine + '[' + proc.host + ']')
                            .click(function () {
                                self.attach(vm, proc.host);
                            })
                        );
                    })
                ),
                $('<a>')
                    .text('spawn in ' + vm.engine)
                    .click(function () {
                        self.spawn(vm, vm.engine);
                    })
            )
        ;
        infoPanes[vm.id] = elem;
        leftPane.append(elem);
    };
    
    self.useVM = function (vm) {
        vmPane.append($('<div>')
            .addClass('vm-desc')
            .click(function () {
                vmPane.fadeOut(400);
                infoPanes[vm.id].fadeIn(400);
            })
            .append($('<div>').text(vm.name))
        );
        self.addInfoPane(vm);
    };
    
    self.spawn = function (vm, engine) {
        account.spawn({ vm : vm.id, engine : engine }, function (proc) {
            vm.processes.push({
                host : proc.host,
                engine : engine,
                vm : vm.id
            });
            
            infoPanes[vm.id].children('.instance-list').append(
                $('<p>').append($('<a>')
                    .text(engine + '[' + proc.host + ']')
                    .click(function () {
                        self.attach(vm, proc.host);
                    })
                )
            );
        });
    };
    
    var windows = {};
    self.attach = function (vm, host) {
        account.attach(host, function (desktop) {
            if (!desktop) {
                console.log('desktop == null');
            }
            else {
                var win = new Window({
                    fb : new FB({ desktop : desktop }),
                    name : vm.name
                });
                var i = Object.keys(windows).length;
                windows[i] = win;
                
                win.on('minimize', function () {
                    if (i in windows) {
                        delete windows[i];
                        account.detach(host);
                        quickBar.push(vm, host);
                    }
                });
                
                win.on('fullscreen', function () {
                    sheet.fadeIn(400);
                    win.element.css('z-index',1000);
                    
                    sheet.click(function () {
                        sheet.fadeOut(400);
                        win.element.css('z-index',100);
                        win.restore();
                    });
                });
                
                win.on('close', function () {
                    if (i in windows) {
                        delete windows[i];
                        account.detach(host);
                    }
                });
                
                win.on('kill', function () {
                    $('.instance-list p a').filter(function () {
                        return $(this).text().match(/\[(.+)\]$/)[1] == host;
                    }).remove();
                    account.kill(host, function () {});
                });
                
                win.on('restart', function () {
                    account.restart(host, function () {});
                });
                
                windowPane.append(win.element);
                Object.keys(windows).forEach(function (k) {
                    windows[k].unfocus();
                });
                
                win.focus();
            }
        });
    };
    
    account.virtualMachines(function (vms) {
        // vms maps vm ids to { vm id, name, filename }
        account.processes(function (procs) {
            // procs maps hosts to { pid, vm id, host, engine }
            Object.keys(vms).forEach(function (vmId) {
                var vm = vms[vmId];
                self.useVM({
                    id : vm.id,
                    name : vm.name,
                    filename : vm.filename,
                    engine : vm.engine, 
                    host : vm.host,
                    processes : Object.keys(procs)
                        .map(function (host) { return procs[host] })
                        .filter(function (p) { return vm.id == p.vm })
                    ,
                });
            });
        });
    });
}

