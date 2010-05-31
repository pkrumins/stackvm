var Connection = require('connection').Connection;
var KeyMapper = require('keymap').KeyMapper;

var VM_Manager = (function () {
    var vms = {};
    var active_vm = null;

    return {
        add: function (vm) {
             vms[vm.vm_id] = vm;
        },
        get: function (id) {
             return vms[id];
        },
        del: function (id) {
             delete vms[id];
        },
        get_active_vm: function () {
             return active_vm;
        },
        set_active_vm: function (vm) {
             active_vm = vm;
        },
    }
})();

exports.VM_Manager = VM_Manager;

function VM_Event_Handler (vm) {
    this.vm = vm;

    this.error = function (msg) {
        $('.center_message', vm.win)
            . show()
            . text(msg.message)
            . css({ color: 'red' });
    }

    this.attached = function (msg) {
        $('.center_message', vm.win).hide();
        vm.event_emitter.redraw_screen();
    }

    this.detached = function (msg) {
        $('.center_message', vm.win)
            . show()
            . text('vm has been detached')
            . css({ color: 'red' });
    }

    this.redraw_screen = function (msg) {
        var img = png_img(msg.png64);
        var x = 0, y = 0;
        var width = parseInt(msg.width, 10);
        var height = parseInt(msg.height, 10);

        update_screen(img, 0, 0, width, height);
        cleanup_images(img);
    }

    this.update_screen = function (msg) {
        var img = png_img(msg.png64);
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
    }

    this.desktop_size = function (msg) {
        vm.win.height(msg.height + 22);
        vm.win.width(msg.width);
    }

    this.copy_rect = function (msg) {
        alert('yeah');
    }

    function png_img (png) {
        // TODO: use MHTML for IE
        return $('<img>').attr('src', 'data:image/png;base64,' + png);
    }

    function cleanup_images (except) {
        $('.console img', vm.win)
            . not(except)
            . remove();
    }
}

function VM_Event_Emitter (vm) {
    this.attach_vm = function () {
        Connection.send_msg({
             vm_id: vm.vm_id,
             action: 'attach'
        });
    }

    this.detach_vm = function () {
        Connection.send_msg({
            vm_id: vm.vm_id,
            action: 'detach'
        });
    }

    this.redraw_screen = function () {
        Connection.send_msg({
             vm_id: vm.vm_id,
             action: 'redraw_screen'
        });
    }

    this.send_key_down = function (key_code) {
        Connection.send_msg({
             vm_id:  vm.vm_id,
             action: 'key_down',
             key: String(KeyMapper.get_key_sym(key_code))
        });
    }

    this.send_key_up = function (key_code) {
        Connection.send_msg({
             vm_id:  vm.vm_id,
             action: 'key_up',
             key: String(KeyMapper.get_key_sym(key_code))
        });
    }

    this.send_pointer = function (x, y, mask) {
        Connection.send_msg({
             vm_id : vm.vm_id,
             action : 'pointer',
             x : String(x),
             y : String(y),
             mask : String(mask)
        });
    }
}

function VM (vm_id) {
    var vm = this;

    this.vm_id  = vm_id;
    this.win = null;
    this.title = null;
    this.event_emitter = new VM_Event_Emitter(this);

    function create_window () {
        var win = $('<div>')
             . addClass('window')
             . attr('tabindex', 0)
             . data('vm_id', vm_id)
             . draggable();

        var title = $('<div>').addClass('title');
        title.append($('<div>').addClass('text').text(vm_id));
        title.append(
            $('<div>').addClass('buttons').append(
                $('<span>').addClass('refresh').append(
                    $('<img>').attr('src', '/img/refresh.png').click(
                        function (ev) {
                            vm.event_emitter.redraw_screen();
                        }
                    )
                )
            ).append(
                $('<span>').addClass('close').append(
                    $('<img>').attr('src', '/img/close.png').click(
                        function (ev) {
                            vm.event_emitter.detach_vm();
                            VM_Manager.del(vm_id);
                            win.remove();
                        }
                    )
                )
            )
        ).append(
            $('<div>').addClass('clear')
        );
        win.append(title);

        var con = $('<div>').addClass('console');
        con.append(
             $('<div>')
                . addClass('center_message')
                . text('Loading ' + vm_id + '...')
        );
        win.append(con);
        
        win.mouseover(function(ev) {
             focus(win);
        });
        
        win.mouseout(function(ev) {
             unfocus(win);
        });
        
        var mouse_mask = 0;
        
        con.mousemove(function(ev) {
             var x = ev.pageX - con.offset().left;
             var y = ev.pageY - con.offset().top;
             vm.event_emitter.send_pointer(x, y, mouse_mask);
        });
        
        con.mousedown(function(ev) {
             mouse_mask = 1;
             vm.event_emitter.send_pointer(ev.pageX, ev.pageY, mouse_mask);
        });
        
        con.mouseup(function(ev) {
             mouse_mask = 0;
             vm.event_emitter.send_pointer(ev.pageX, ev.pageY, mouse_mask);
        });
        
        win.keydown(function(ev) {
             vm.event_emitter.send_key_down(ev.keyCode);
             ev.preventDefault();
        });
        
        win.keyup(function(ev) {
             vm.event_emitter.send_key_up(ev.keyCode);
             ev.preventDefault();
        });

        $('#content').append(win);
        return win;
    }

    function focus (win) {
        VM_Manager.set_active_vm(win);
        win.focus();
        $('.title', win).addClass("active-title");
    }

    function unfocus (win) {
        VM_Manager.set_active_vm(null);
        $('.focusremover').focus();
        $('.title', win).removeClass("active-title");
    }

    this.focus = function () {
        focus(this.win);
    }

    this.unfocus = function () {
        unfocus(this.win);
    }

    this.run = function () {
        this.win = create_window();
        Connection.add_event_handler(new VM_Event_Handler(this));
        vm.event_emitter.attach_vm();
    }
}

exports.VM = VM;

