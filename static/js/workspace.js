function Workspace (rootElem) {
    if (!(this instanceof Workspace)) return new Workspace(rootElem);
    
    rootElem.empty();
    var leftPane = $('<div>').attr('id','left-pane');
    rootElem.append(leftPane);
    
    this.useVM = function (vmName) {
        leftPane.append($('<div>')
            .addClass('vm-desc')
            .text(vmName)
        );
    };
}

