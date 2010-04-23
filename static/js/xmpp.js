var XMPP = (function() {
  var xmpp_connection = null;
  var really_connected = false;     // can't rely on xmpp_connection.connected

  function connected() {
    return really_connected;
  }

  function connect() {
    xmpp_connection = new Strophe.Connection('/http-bind');
    xmpp_connection.connect('vm@localhost', 'password', onConnect);
    xmpp_connection.addHandler(onMessage, null, 'message');
  }

  function log(msg) {
    console.log(msg);
  }

  function onConnect(status) {
    if (status == Strophe.Status.CONNECTING) {
      log('Strophe is connecting.');
    }
    else if (status == Strophe.Status.CONNECTED) {
      log('Strophe connected.');

      really_connected = true;
      for (i in msg_queue) {
        xmpp_connection.send(msg_queue[i]);
      }
      msg_queue = [];
    }
    else if (status == Strophe.Status.CONNFAIL) {
      really_connected = false;
      log('Strophe failed to connect.');
    }
    else if (status == Strophe.Status.DISCONNECTING) {
      really_connected = false;
      log('Strophe is disconnecting.');
    }
    else if (status == Strophe.Status.DISCONNECTED) {
      really_connected = false;
      log('Strophe disconnected.');
    }
    else if (status == Strophe.Status.AUTHENTICATING) {
      log('Strophe is authenticating.')
    }
    else if (status == Strophe.Status.AUTHFAIL) {
      log('Failed authenticating.')
    }
  }

  function onMessage(msg) {
    jmsg = $(msg);
    vm_id = jmsg.find('vm_id').text();
    action = jmsg.find('action').text();
    handler = find_handler(vm_id);
    if (!handler) {
      log("Unknown vm '" + vm_id + "'");
      return;
    }
    action_fn = handler[action];
    if (!action_fn) {
      log("Handler function for action '" + action + "' not found!");
      return;
    }
    handler[action](jmsg);
    return true; // must have - if removed, removes the onMessage callback from xmpp
  }

  msg_queue = [];
  function send_msg(msg) {
    if (!connected()) {
      msg_queue.push(msg);
      connect();
    }
    else {
      xmpp_connection.send(msg);
    }
  }

  var handlers = {};
  function find_handler(vm_id) {
    return handlers[vm_id];
  }

  return {
    add_event_handler: function(handler) {
      handlers[handler.vm.vm_id] = handler;
    },
    del_event_handler: function(vm_id) {
      delete handlers[vm_id];
    },
    send_msg: function(msg) {
      send_msg(msg);
    }
  }
})();

