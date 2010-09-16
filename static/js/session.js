function Session (cb) {
    DNode().connect(
        { ping : 2000, timeout : 100 },
        function (remote, conn) {
            remote.authenticate(function (err, account) {
                if (err) cb(err);
                else cb(null, new UI(account));
            });
            
            function reconnect () {
                if (window.console) console.log('reconnecting');
                conn.reconnect(3000, function f (err) {
                    if (err) {
                        if (window.console) console.log(err);
                        reconnect();
                    }
                    else {
                        if (window.console) console.log('reconnected');
                    }
                });
            }
            
            conn.on('timeout', reconnect);
            conn.on('end', reconnect);
        }
    );
}

