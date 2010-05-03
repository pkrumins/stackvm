var Connection = (function() {
  var connection = null;
  var really_connected = false;
  var msg_queue = [];
  var event_handlers = {};

  function connected() {
    return really_connected;
  }

  function connect() {
    connection = new io.Socket('10.1.1.2', {rememberTransport: false, port:9000});
    connection.addEvent('connect', function() {
      really_connected = true;
      process_msg_queue();
    });
    connection.addEvent('disconnect', function() {
      really_connected = false;
    });
    connection.addEvent('message', function(msg) {
      dispatch_msg(msg);
    });
    connection.connect();
  }

  function dispatch_msg(msg) {
    var msg = JSON.parse(msg);
    var handler = find_event_handler(msg.vm_id);
    if (!handler) {
      console.log("Unknown VM '" + msg.vm_id + "'");
      return;
    }
    actionfn = handler[msg.action];
    if (!actionfn) {
      console.log("Unknown action '" + msg.action + "' for VM '" + msg.vm_id + "'");
      return;
    }
    actionfn(msg);
  }

  function process_msg_queue() {
    for (var i = 0; i < msg_queue.length; i++) {
      send_msg(msg_queue[i]);
    }
    msg_queue = [];
  }

  function send_msg(msg) {
    if (!connected()) {
      msg_queue.push(msg);
      connect();
    }
    else {
      connection.send(msg);
    }
  }

  function find_event_handler(vm_id) {
    return event_handlers[vm_id];
  }

  return {
    add_event_handler: function(handler) {
      event_handlers[handler.vm.vm_id] = handler;
    },

    del_event_handler: function(vm_id) {
      delete event_handlers[vm_id];
    },

    send_msg: function(msg) {
      send_msg(msg);
    }
  }
})();

