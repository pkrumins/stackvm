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
    
    this.useVM = function (vmName) {
        selectPane.append($('<div>')
            .addClass('vm-desc')
            .click(function () {
                self.attach(vmName);
            })
            .append($('<div>').text(vmName))
        );
    };
    
    this.attach = function (vmName) {
        //var vm = account.attach(vmName);
        var fb = new FB({});
        windowPane.append($('<div>')
            .css({
                position : 'absolute',
                top : '100px',
                left : '100px'
            })
            .append(fb.element)
        );
    };
    
    account.vmList(function (vmList) {
        vmList.forEach(self.useVM);
    });
}

