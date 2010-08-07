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
    
    var chats = {};
    sidebar.on('chat', function (contact) {
        if (contact.name in chats) return;
        var chat = new ChatWindow;
        var rightMost = Math.min.apply({}, [$(window).width()].concat(
            Object.keys(chats).map(function (name) {
                return chats[name].element.offset().left;
            })
        ));
        workspace.element.append(chat.element);
        console.log(rightMost);
        chat.element.offset({
            left : rightMost - 210,
            top : $(window).height() - 210
        });
        chats[contact.name] = chat;
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

