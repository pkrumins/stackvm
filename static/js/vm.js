var Connection = require('connection').Connection;
var KeyMapper = require('keymap').KeyMapper;
var Display = require('display');

var Manager = (function () {
    var vms = {};
    var activeVm = null;

    return {
        add: function (vm) {
             vms[vm.vmId] = vm;
        },
        get: function (id) {
             return vms[id];
        },
        del: function (id) {
             delete vms[id];
        },
        getActiveVm: function () {
             return activeVm;
        },
        setActiveVm: function (vm) {
             activeVm = vm;
        },
    }
})();

exports.Manager = Manager;

function VmEventHandler (vm) {
    this.vm = vm;

    this.error = function (msg) {
        vm.display.consoleMessage(msg.message);
    }

    this.attached = function (msg) {
        vm.display.consoleMessage(null);
    }

    this.detached = function (msg) {
        vm.dipslay.consoleMessage('vm has been detached');
    }

    this.updateScreen = function (msg) {
        vm.display.conDraw(msg.image64, msg.imageType, msg.x, msg.y,
            msg.width, msg.height, msg.fullScreen);
    }

    this.desktopSize = function (msg) {
        vm.display.resize(msg.width, msg.height);
    }

    this.copyRect = function (msg) {
        vm.display.copyRect(msg.srcX, msg.srcY, msg.dstX, msg.dstY,
            msg.width, msg.height);
    }

    this.screenshot = function (msg) {
        console.log(msg.screenshotUrl);
    }

    this.screencastStarted = function (msg) {
        console.log('screencast started');
    }

    this.screencastEnded = function (msg) {
        console.log(msg.screencastUrl);
    }
}

function VmEventEmitter (vm) {
    this.attachVm = function () {
        Connection.sendMsg({
             vmId: vm.vmId,
             action: 'attach'
        });
    }

    this.detachVm = function () {
        Connection.sendMsg({
            vmId: vm.vmId,
            action: 'detach'
        });
    }

    this.redrawScreen = function () {
        Connection.sendMsg({
             vmId: vm.vmId,
             action: 'redrawScreen'
        });
    }

    this.sendKeyDown = function (keyCode) {
        Connection.sendMsg({
             vmId:  vm.vmId,
             action: 'keyDown',
             key: String(KeyMapper.getKeySym(keyCode))
        });
    }

    this.sendKeyUp = function (keyCode) {
        Connection.sendMsg({
             vmId:  vm.vmId,
             action: 'keyUp',
             key: String(KeyMapper.getKeySym(keyCode))
        });
    }

    this.sendPointer = function (x, y, mask) {
        Connection.sendMsg({
             vmId : vm.vmId,
             action : 'pointer',
             x : String(x),
             y : String(y),
             mask : String(mask)
        });
    }

    this.takeScreenshot = function () {
        Connection.sendMsg({
            vmId : vm.vmId,
            action : 'takeScreenshot'
        });
    }

    this.startScreencast = function () {
        Connection.sendMsg({
            vmId : vm.vmId,
            action : 'startScreencast'
        });
    }

    this.stopScreencast = function () {
        Connection.sendMsg({
            vmId : vm.vmId,
            action : 'stopScreencast'
        });
    }
}

function VM (vmId) {
    var vm = this;

    this.vmId = vmId;
    this.eventEmitter = new VmEventEmitter(this);
    //this.display = new Display.StackedDisplay(vm, {width: 400, height: 200});
    this.display = new Display.CanvasDisplay(vm, {width: 400, height: 200});

    function createWindow () {
        var win = vm.display.createWindow();
        $('#content').append(win);
    }

    this.focus = function () {
        Manager.setActiveVm(vm);
        vm.display.focus();
    }

    this.unfocus = function () {
        Manager.setActiveVm(null);
        vm.display.unfocus();
    }

    this.run = function () {
        createWindow();
        Connection.addEventHandler(new VmEventHandler(this));
        vm.eventEmitter.attachVm();
    }
}

exports.VM = VM;

