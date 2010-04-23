var VM_Manager = (function() {
  var vms = {};

  return {
    add: function(vm) {
      vms[vm.vm_id] = vm;
    },
    get: function(id) {
      return vms[id];
    },
    del: function(id) {
      delete vms[id];
    }
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

  this.update_screen = function(msg) {
    console.log('update_screen');
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
    return $msg({to: 'vm.localhost'}).c('vm_id').t(vm.vm_id).up()
  }

  this.start_vm = function() {
    XMPP.send_msg(
        prepare_msg().
        c('action').t('start_vm')
    );
  }
  this.send_key_down = function(key_code) {
    // TODO: see the flush() method in strophe.js, seems like
    // strophe.js queues events up to 100ms, and we can't have 100ms
    // lag for sending each key!
    XMPP.send_msg(
        prepare_msg().
        c('action').t('key_down').up().
        c('key').t(key_code)
    );
  }
  this.send_key_up = function(key_code) {
    XMPP.send_msg(
        prepare_msg().
        c('action').t('key_down').up().
        c('key').t(key_code)
    );
  }
}

function VM(vm_id) {
  this.vm_id  = vm_id;
  this.win = null;
  var event_emitter = new VM_Event_Emitter(this);

  function create_window() {
    var win = $('<div>').
                 data('vm_id', vm_id).
                 attr('id', vm_id + '_window').
                 attr('class', 'window').
                 height(400). // while testing
                 width(400);
    $('#content').append(win);
    win.keydown(function(ev) {
        alert('foo');
      event_emitter.send_key_down(ev.keyCode);
    });
    win.keyup(function(ev) {
      event_emitter.send_key_up(ev.keyCode);
    });
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

