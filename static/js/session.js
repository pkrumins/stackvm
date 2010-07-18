function Session (workspace, account) {
    if (!(this instanceof Session)) return new Session(workspace, account);
    
    account.vmList(function (vmList) {
        vmList.forEach(workspace.useVM);
    });
}

