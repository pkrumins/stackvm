var Connection = require('connection').Connection;
var KeyMapper = require('keymap').KeyMapper;

// Hacking on this now, just named it SimpleDisplay for now until
// I know what the interface is for <canvas> and <svg> displays
var SimpleDisplay = require('display').SimpleDisplay;

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
        $('.centerMessage', vm.win)
            .show()
            .text(msg.message)
            .css({ color: 'red' });
    }

    this.attached = function (msg) {
        $('.centerMessage', vm.win).hide();
        vm.eventEmitter.redrawScreen();
    }

    this.detached = function (msg) {
        $('.centerMessage', vm.win)
            .show()
            .text('vm has been detached')
            .css({ color: 'red' });
    }

    this.updateScreen = function (msg) {
        var img = pngImg(msg.png64);
        var x = parseInt(msg.x, 10);
        var y = parseInt(msg.y, 10);
        var width = parseInt(msg.width, 10);
        var height = parseInt(msg.height, 10);

        if (height > vm.win.height() + 22) {
             vm.win.height(height+22); // 22 to account for window's .title.
        }
        if (width > vm.win.width()) {
             vm.win.width(width);
        }
        img.css({
             position : 'absolute',
             left : x,
             top : y,
             width : width,
             height : height
        });
        $('.console', vm.win).append(img);
        if (msg.fullScreen) cleanupImages(img);
    }

    this.desktopSize = function (msg) {
        vm.win.height(msg.height + 22);
        vm.win.width(msg.width);
    }

    this.copyRect = function (msg) {
        alert('yeah');
    }

    function pngImg (png) {
        // TODO: use MHTML for IE
        return $('<img>').attr('src', 'data:image/png;base64,' + png);
    }

    function cleanupImages (except) {
        $('.console img', vm.win)
            .not(except)
            .remove();
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
    this.win = null;
    this.display = new SimpleDisplay(vm, {width: 400, height: 200});

    function createWindow () {
        var win = vm.display.createWindow();
        $('#content').append(win);
        return win;
    }

    function focus (win) {
        Manager.setActiveVm(win);
        win.focus();
        $('.title', win).addClass("activeTitle");
    }

    function unfocus (win) {
        Manager.setActiveVm(null);
        $('.focusremover').focus();
        $('.title', win).removeClass("activeTitle");
    }

    this.focus = function () {
        focus(this.win);
    }

    this.unfocus = function () {
        unfocus(this.win);
    }

    this.run = function () {
        this.win = createWindow();
        Connection.addEventHandler(new VmEventHandler(this));
        vm.eventEmitter.attachVm();
    }
}

exports.VM = VM;

