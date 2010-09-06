function Session (params, cb) {
    DNode().connect(function (remote, conn) {
        remote.authenticate(params, function (account) {
            cb(account ? new UI(account) : null);
        });
    });
}

