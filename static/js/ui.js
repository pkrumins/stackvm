function UI (account) {
    if (!(this instanceof UI)) return UI(account);
    var contacts = account.contacts;
    var processes = account.processes;
    
    var workspace = new Workspace;
    var sidebar = new SideBar;
    var taskbar = new TaskBar;
    
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
    contacts.list(function (list) {
        console.log('contact list:');
        console.dir(list);
    });
    
    // --
    
    this.element = workspace.element;
}

