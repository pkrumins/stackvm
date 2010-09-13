function Session (cb) {
    DNode().connect(function (remote, conn) {
        remote.authenticate(function (err, account) {
            if (err) console.error(err);
            else cb(new UI(account));
        });
    });
}

