function UI (account) {
    if (!(this instanceof UI)) return UI(account);
    var contacts = account.contacts;
    var processes = account.processes;
    
    var workspace = new Workspace;
    var sidebar = new SideBar({ engines : ['qemu','vmware'] });
    var taskbar = new TaskBar;
    
    workspace.element.append(
        sidebar.element,
        taskbar.element
    );
    
    // inter-dependent ui hooks:
    workspace.on('minimize', function (win) {
        taskbar.push(win);
    });
    
    sidebar.on('attach', function (proc) {
        console.log('proc:')
        console.dir(proc);
        // workspace.attachWindow(win);
    });
    
    taskbar.on('pop', function (win) {
        workspace.attachWindow(win);
    });
    
    // external resource hooks:
    contacts.on('offline', function (who) {
        sidebar.updateContact(who, 'offline');
    });
    
    contacts.on('online', function (who) {
        sidebar.updateContact(who, 'online');
    });
    
    processes.on('spawn', function (proc) {
        console.log('spawn!');
        sidebar.addInstance(proc);
    });
    
    processes.on('exit', function (addr) {
        console.log('exit: ' + addr);
        sidebar.removeInstance(addr);
    });
    
    // fetch some lists:
    contacts.list(function (list) {
        list.forEach(sidebar.addContact);
    });
    
    processes.list(function (list) {
        list.forEach(sidebar.addDisk);
    });
    
    // --
    
    this.element = workspace.element;
}

