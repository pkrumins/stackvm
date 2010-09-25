$(document).ready(function () {
    DNode.connect(function (remote) {
        remote.session(function (err, account) {
            if (account) {
                Hash(account.disks).forEach(function (disk, filename) {
                    console.log(filename);
                });
            }
        });
    });
});
