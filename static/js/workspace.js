function Workspace (rootElem, account) {
    if (!(this instanceof Workspace))
        return new Workspace(rootElem, account);
    var self = this;
    
    $('form#login').fadeOut(400);
    
    var selectPane = $('<div>')
        .addClass('left-pane')
        .attr('id','select-pane')
        .hide()
        .fadeIn(400);
    ;
    rootElem.append(selectPane);
    
    var windowPane = $('<div>')
        .attr('id','window-pane')
        .hide()
        .fadeIn(400)
    ;
    rootElem.append(windowPane);
    
    var infoPanes = {};
    self.addInfoPane = function (vm) {
        var elem = $('<div>')
            .hide()
            .addClass('left-pane')
            .attr('id','info-pane')
            .append(
                $('<div>')
                    .addClass('back')
                    .text('back')
                    .click(function () {
                        selectPane.fadeIn(400);
                        elem.fadeOut(400);
                    })
                ,
                $('<p>').text(vm.name),
                $('<a>')
                    .text('spawn in qemu')
                    .click(function () {
                        account.spawn(
                            { vm : vm.id, engine : 'qemu' },
                            function (port) {
                                vm.processes.push({
                                    port : port,
                                    engine : 'qemu',
                                    vm : vm.id
                                });
                            }
                        );
                    })
            )
        ;
        infoPanes[vm.id] = elem;
        rootElem.append(elem);
    };
    
    self.useVM = function (vm) {
        selectPane.append($('<div>')
            .addClass('vm-desc')
            .click(function () {
                selectPane.fadeOut(400);
                infoPanes[vm.id].fadeIn(400);
            })
            .append($('<div>').text(vm.name))
        );
        self.addInfoPane(vm);
    };
    
    var windows = {};
    self.attach = function (port) {
        account.attach(port, function (vm) {
            if (!vm) {
                console.log('vm == null');
            }
            else {
                var fb = new FB({ vm : vm });
                var win = new Window({ fb : fb, name : vmName });
                windows[vmName] = win;
                windowPane.append(win.element);
                Object.keys(windows).forEach(function (w) {
                    windows[w].unfocus();
                });
                win.focus();
            }
        });
    };
    
    account.virtualMachines(function (vms) {
        // vms maps vm ids to { vm id, name, filename }
        account.processes(function (procs) {
            // procs maps ports to { pid, vm id, port, engine }
            Object.keys(vms).forEach(function (vmId) {
                var vm = vms[vmId];
                self.useVM({
                    id : vm.id,
                    name : vm.name,
                    filename : vm.filename,
                    processes : Object.keys(procs)
                        .map(function (port) { return procs[port] })
                        .filter(function (p) { return vm.id == p.vm })
                    ,
                });
            });
        });
    });
}

