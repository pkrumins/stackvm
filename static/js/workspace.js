function Workspace (rootElem, account) {
    if (!(this instanceof Workspace))
        return new Workspace(rootElem, account);
    var self = this;
    
    rootElem.empty();
    var leftPane = $('<div>').attr('id','left-pane');
    rootElem.append(leftPane);
    
    this.useVM = function (vmName) {
        leftPane.append($('<div>')
            .addClass('vm-desc')
            .text(vmName)
            .click(function () {
                self.attach(vmName);
            })
        );
    };
    
    this.attach = function () {
        // 
    };
    
    account.vmList(function (vmList) {
        vmList.forEach(self.useVM);
    });
}

