var VM_Manager = (function() {
  var vms = {};
  var active_vm = null;

  return {
    add: function(vm) {
      vms[vm.vm_id] = vm;
    },
    get: function(id) {
      return vms[id];
    },
    del: function(id) {
      delete vms[id];
    },
    get_active_vm: function() {
      return active_vm;
    },
    set_active_vm: function(vm) {
      active_vm = vm;
    },
  }
})();

function VM_Event_Handler(vm) {
  this.vm = vm;

  this.error = function(msg) {
    console.log("Error: " + msg.error);
  }

  this.connected = function(msg) {
    $('.loading', vm.win).hide();
  }

  function png_img(png) {
    // TODO: use MHTML for IE
    return $('<img>').attr('src', 'data:image/png;base64,' + png);
  }

  this.redraw_screen = function(msg) {
    var width  = parseInt(msg.width);
    var height = parseInt(msg.height);
    var img = png_img(msg.png);
    vm.win.width(width);
    vm.win.height(height + 22);
    $('.console', vm.win).append(img);
  }

  this.update_screen = function(msg) {
    var width  = msg.width;
    var height = msg.height;
    var img = png_img(msg.png);
    img.css({
      position: 'absolute',
      left:     parseInt(msg.x),
      top:      parseInt(msg.y),
      width:    parseInt(msg.width),
      height:   parseInt(msg.height),
    });
    $('.console', vm.win).append(img);
  }

  this.disconnect = function(msg) {
    console.log('vm ' + msg.vm_id + ' disconnected');
  }
}

function VM_Event_Emitter(vm) {
  this.vm = vm;

  function prepare_msg() {
    return $msg({to: 'vm.localhost'}).c('vm_id').t(vm.vm_id).up()
  }

  function keymap(code) {
    var syms = {
      8 :  0xff00 + 8,  // backspace
      13 : 0xff00 + 13, // return
      17 : 0xffe4,      // left control
      18 : 0xff00 + 18, // left shift
      191 : 47
    };
    return syms[code] || code;
  }

  this.start_vm = function() {
    Connection.send_msg({
      vm_id:  vm.vm_id,
      action: 'start_vm'
    });
  }

  this.send_key_down = function(key_code) {
    console.log('sending ' + keymap(key_code));
    Connection.send_msg({
      vm_id:  vm.vm_id,
      action: 'key_down',
      key:    String(keymap(key_code))
    });
  }

  this.send_key_up = function(key_code) {
    Connection.send_msg({
      vm_id:  vm.vm_id,
      action: 'key_up',
      key:    String(keymap(key_code))
    });
  }
}

function VM(vm_id) {
  this.vm_id  = vm_id;
  this.win = null;
  var event_emitter = new VM_Event_Emitter(this);

  function create_window() {
    var win = $('<div class="window">').
                 attr('tabindex', 0).  // needed for keydown/keyup
                 data('vm_id', vm_id).
                 width(400).
                 height(200).
                 draggable();

    win.append($('<div class="title">').text(vm_id));
    var console = $('<div class="console">');
    console.append(
      $('<div class="loading">').css({
        'margin-top':   (200-20)/2-10 + 'px',
        'text-align':   'center',
      }).
      text('Loading ' + vm_id + '...')
    );
    win.append(console);

    win.mouseover(function(ev) {
      focus(win);
    });
    win.mouseout(function(ev) {
      unfocus(win);
    });
    win.keydown(function(ev) {
      event_emitter.send_key_down(ev.keyCode);
    });
    win.keyup(function(ev) {
      event_emitter.send_key_up(ev.keyCode);
    });

    $('#content').append(win);
    return win;
  }

  function focus(win) {
    VM_Manager.set_active_vm(win);
    win.focus();
    $('.title', win).addClass("active-title");
  }

  function unfocus(win) {
    VM_Manager.set_active_vm(null);
    $('.focusremover').focus();
    $('.title', win).removeClass("active-title");
  }

  this.focus = function() {
    focus(this.win);
  }

  this.unfocus = function() {
    unfocus(this.win);
  }

  this.run = function() {
    this.win = create_window();
    Connection.add_event_handler(new VM_Event_Handler(this));
    event_emitter.start_vm();
  }
}

