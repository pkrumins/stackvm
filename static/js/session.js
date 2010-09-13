function Session (cb) {
    DNode().connect(function (remote, conn) {
        remote.authenticate(function (err, account) {
            if (err) cb(err);
            else cb(null, new UI(account));
        });
    });
}

