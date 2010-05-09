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
    console.log("beep");
    return $('<img>').attr('src', 'data:image/png;base64,' + png);
  }

  function cleanup_images(except) {
    $('.console img', vm.win)
      .not(except)
      .remove();
  }

  function update_screen(img, x, y, w, h) {
    var console = $('.console', vm.win);
    if (h > parseInt(vm.win.height())+22) {
      vm.win.height(h+22); // 22 to account for window's .title.
    }
    if (w > parseInt(vm.win.width())) {
      vm.win.width(w);
    }
    img.css({
      position: 'absolute',
      left:     x,
      top:      y,
      width:    w,
      height:   h
    });
    console.append(img);
  }

  this.redraw_screen = function(msg) {
    var img = png_img(msg.png);
    update_screen(img, 0, 0, parseInt(msg.width), parseInt(msg.height));
    cleanup_images(img);
  }

  this.update_screen = function(msg) {
    var img = png_img(msg.png);
    update_screen(img, parseInt(msg.x), parseInt(msg.y),
      parseInt(msg.width), parseInt(msg.height));
  }

  this.disconnect = function(msg) {
    console.log('vm ' + msg.vm_id + ' disconnected');
  }
}

function VM_Event_Emitter(vm) {
  this.start_vm = function() {
    Connection.send_msg({
      vm_id:  vm.vm_id,
      action: 'start_vm'
    });
  }

  this.redraw_screen = function() {
    Connection.send_msg({
      vm_id:  vm.vm_id,
      action: 'redraw_screen'
    });
  }

  this.send_key_down = function(key_code) {
    Connection.send_msg({
      vm_id:  vm.vm_id,
      action: 'key_down',
      key:    String(KeyMapper.get_key_sym(key_code))
    });
  }

  this.send_key_up = function(key_code) {
    Connection.send_msg({
      vm_id:  vm.vm_id,
      action: 'key_up',
      key:    String(KeyMapper.get_key_sym(key_code))
    });
  }
}

function VM(vm_id) {
  this.vm_id  = vm_id;
  this.win = null;
  this.title = null;
  var event_emitter = new VM_Event_Emitter(this);

  function create_window() {
    var win = $('<div>')
      . addClass('window')
      . attr('tabindex', 0)
      . data('vm_id', vm_id)
      . width(400)
      . height(200)
      . draggable();

    var title = $('<div>').addClass('title');
    title.append($('<div>').addClass('text').text(vm_id));
    title.append(
      $('<div>').addClass('buttons').append(
        $('<div>').addClass('refresh').append(
          $('<img>').attr('src', '/img/refresh.png').click(
            function(ev) {
              event_emitter.redraw_screen();
            }
          )
        )
      )
    ).append($('<div>').addClass('clear'));
    win.append(title);

    var con = $('<div>').addClass('console');
    con.append(
      $('<div>').addClass('loading').css({
        'margin-top': (200-20)/2-10 + 'px',
        'text-align': 'center',
      }).
      text('Loading ' + vm_id + '...')
    );
    win.append(con);

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

