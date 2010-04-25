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
    console.log("Error: " + msg.find('error').text());
  }

  this.connect = function(msg) {
    console.log('stackvm connected');
  }

  this.redraw_screen = function(msg) {
    var png = msg.find('png').text();
    var img = $('<img>').attr('src', 'data:image/png;base64,' + png);
    vm.win.append(img);
  }

  this.disconnect = function(msg) {
    console.log('stackvm disconnected');
  }
}

function VM_Event_Emitter(vm) {
  this.vm = vm;

  function prepare_msg() {
    return $msg({to: 'vm.localhost'}).
             c('vm_id').t(vm.vm_id).up()
  }

  function keymap(code) {
      var syms = {
          8 :  0xff00 + 8,  // backspace
          13 : 0xff00 + 13, // return
          17 : 0xffe4,      // left control
          18 : 0xff00 + 18, // left shift
          191 : 47
      };
      return String(syms[code] || code);
  }


  this.start_vm = function() {
    XMPP.send_msg(
        prepare_msg().
        c('action').t('start_vm')
    );
  }
  this.send_key_down = function(key_code) {
    console.log('sending ' + keymap(key_code));
    XMPP.send_msg(
        prepare_msg().
        c('action').t('key_down').up().
        c('key').t(keymap(key_code)),
        true
    );
  }
  this.send_key_up = function(key_code) {
    XMPP.send_msg(
        prepare_msg().
        c('action').t('key_up').up().
        c('key').t(keymap(key_code)),
        true
    );
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
                 draggable().
                 width(640).
                 height(480);

    win.append($('<div class="title">').text(vm_id));
    win.append($('<div class="console">'));

    win.mouseover(function(ev) {
        VM_Manager.set_active_vm(this);
        $('.title', win).addClass("active-title");
    });
    win.mouseout(function(ev) {
        VM_Manager.set_active_vm(null);
        $('.title', win).removeClass("active-title");
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

  this.run = function() {
    this.win = create_window();
    XMPP.add_event_handler(new VM_Event_Handler(this));
    event_emitter.start_vm();
  }

  this.send_key_down = function(key_code) {
    event_emitter.send_key_down(key_code);
  }

  this.send_key_up = function(key_code) {
    event_emitter.send_key_up(key_code);
  }
}

