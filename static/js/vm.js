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
        var img = pngImg(msg.png64);
        vm.display.conDraw(img, msg.x, msg.y, msg.width, msg.height,
            msg.fullScreen);
    }

    this.desktopSize = function (msg) {
        vm.display.resize(msg.height, msg.width);
    }

    this.copyRect = function (msg) {
        alert('copyRect incoming');
    }

    function pngImg (png) {
        // TODO: use MHTML for IE
        return $('<img>').attr('src', 'data:image/png;base64,' + png);
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
}

function VM (vmId) {
    var vm = this;

    this.vmId = vmId;
    this.eventEmitter = new VmEventEmitter(this);
    this.display = new Display.StackedDisplay(vm, {width: 400, height: 200});
    //this.display = new Display.CanvasDisplay(vm, {width: 400, height: 200});

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

