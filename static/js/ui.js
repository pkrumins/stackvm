function UI (account) {
    if (!(this instanceof UI)) return UI(account);
    
    var workspace = new Workspace;
    var sidebar = new SideBar;
    var taskbar = new TaskBar;
    
    workspace.on('minimize', function (win) {
        taskbar.push(win);
    });
    
    sidebar.on('attach', function (win) {
        workspace.attachWindow(win);
    });
    
    taskbar.on('pop', function (win) {
        workspace.attachWindow(win);
    });
    
    this.element = workspace.element;
}

