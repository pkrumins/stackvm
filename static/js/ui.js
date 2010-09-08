function UI (account) {
    if (!(this instanceof UI)) return UI(account);
    var contacts = account.contacts;
    var disks = account.disks;
    
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
        var win = new Window({ remoteFB : proc.fb, proc : proc });
        workspace.attachWindow(win);
    });
    
    workspace.on('attach', function (proc) {
        var win = new Window({ remoteFB : proc.fb, proc : proc });
        workspace.attachWindow(win);
    });
    
    sidebar.on('chat', function (contact) {
        if (workspace.hasChat(contact.name)) return;
        var chat = new ChatWindow(account.name, contact);
        workspace.addChat(chat);
    });
     
    taskbar.on('pop', function (win) {
        workspace.attachWindow(win);
    });
    
    Hash(disks).forEach(sidebar.addDisk);
    Hash(contacts).forEach(function (contact) {
        contact.subscribe(function (sub) {
            sub.on('message', function (msg) {
                if (!workspace.hasChat(contact.name)) {
                    var chat = new ChatWindow(account.name, contact);
                    workspace.addChat(chat);
                }
                workspace.routeChat(contact, msg);
            });
            sub.on('share', function (type, res) {
                if (!workspace.hasChat(contact.name)) {
                    var chat = new ChatWindow(account.name, contact);
                    workspace.addChat(chat);
                }
                workspace.routeResource(contact, type, res);
            });
    
        });
        sidebar.addContact(contact);
    });
    
    this.element = workspace.element;
}

