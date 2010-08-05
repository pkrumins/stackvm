module.exports = {
    qemu : new (require('./managers/qemu')),
    vmware : new (require('./managers/vmware')),
};
