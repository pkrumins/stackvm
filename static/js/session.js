function Session (user, pass, cb) {
    var contacts = new EventEmitter;
    DNode.expose(contacts, 'emit');
    
    var processes = new EventEmitter;
    DNode.expose(processes, 'emit');
    
    DNode().connect(function (remote, conn) {
        remote.authenticate(user, pass, function (account) {
            cb(account ? new UI(account) : null);
        });
    });
}

