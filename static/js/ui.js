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
        proc.attach(function (fb) {
            var win = new Window({ remoteFB : fb, proc : proc });
            workspace.attachWindow(win);
        });
    });
    
    workspace.on('attach', function (vm) {
        var win = new Window({
            remoteFB : vm.fb,
            proc : {
                addr : vm.addr,
                shared : true,
                name : vm.nam
            }
        });
        workspace.attachWindow(win);
    });
    
    sidebar.on('chat', function (contact) {
        if (workspace.hasChat(contact.name)) return;
        var chat = new ChatWindow({
            me : account.user.name,
            contact : contact
        });
        workspace.addChat(chat);
    });
     
    contacts.on('share', function (vm) {
        if (!workspace.hasChat(vm.from.name)) {
            var chat = new ChatWindow({
                me : account.user.name,
                contact : msg.from
            });
            workspace.addChat(chat);
        }
        workspace.routeResource(vm);
    });
    
    contacts.on('message', function (msg) {
        if (!workspace.hasChat(msg.from.name)) {
            var chat = new ChatWindow({
                me : account.user.name,
                contact : msg.from
            });
            workspace.addChat(chat);
        }
        workspace.routeChat(msg);
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
        sidebar.addInstance(proc);
    });
    
    processes.on('exit', function (addr) {
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

