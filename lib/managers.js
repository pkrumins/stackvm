module.exports = {
    qemu : new (require(__dirname + '/managers/qemu')),
    vmware : new (require(__dirname + '/managers/vmware')),
};
