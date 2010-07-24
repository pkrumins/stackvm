function Workspace (rootElem, account) {
    if (!(this instanceof Workspace))
        return new Workspace(rootElem, account);
    var self = this;
    
    $('form#login').fadeOut(400);
    
    var selectPane = $('<div>')
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
    
    self.useVM = function (vm) {
        selectPane.append($('<div>')
            .addClass('vm-desc')
            .click(function () {
                self.attach(vm.name);
            })
            .append($('<div>').text(vm.name))
            .append($('<p>').text(vm.processes.length))
        );
    };
    
    var windows = {};
    self.attach = function (vmName) {
        account.attach(vmName, function (vm) {
            var fb = new FB({ vm : vm });
            var win = new Window({ fb : fb, name : vmName });
            windows[vmName] = win;
            windowPane.append(win.element);
            Object.keys(windows).forEach(function (w) {
                windows[w].unfocus();
            });
            win.focus();
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

