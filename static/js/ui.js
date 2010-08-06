function UI (account) {
    if (!(this instanceof UI)) return UI(account);
    var contacts = account.contacts;
    var processes = account.processes;
    
    var workspace = new Workspace;
    var sidebar = new SideBar;
    var taskbar = new TaskBar;
    
    workspace.element.append(
        sidebar.element,
        taskbar.element
    );
    
    // inter-dependent ui hooks:
    workspace.on('minimize', function (win) {
        taskbar.push(win);
    });
    
    sidebar.on('attach', function (win) {
        workspace.attachWindow(win);
    });
    
    taskbar.on('pop', function (win) {
        workspace.attachWindow(win);
    });
    
    // external resource hooks:
    
    // fetch some lists:
    contacts.list(function (list) {
        console.dir(list);
        list.forEach(sidebar.addContact);
    });
    
    processes.list(function (list) {
        console.dir(list);
    });
    
    // --
    
    this.element = workspace.element;
}

