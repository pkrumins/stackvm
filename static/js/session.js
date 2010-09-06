function Session (params, cb) {
    var contacts = new EventEmitter;
    DNode.expose(contacts, 'emit');
    
    var processes = new EventEmitter;
    DNode.expose(processes, 'emit');
    
    DNode().connect(function (remote, conn) {
        remote.authenticate(params, function (account) {
            cb(account ? new UI(account) : null);
        });
    });
}

