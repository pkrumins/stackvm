var XMPP = (function() {
  var xmpp_connection = null;

  function connected() {
    return xmpp_connection && xmpp_connection.connected;
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
      for (i in msg_queue) {
        xmpp_connection.send(msg_queue[i]);
      }
      msg_queue = [];
    }
    else if (status == Strophe.Status.CONNFAIL) {
      log('Strophe failed to connect.');
    }
    else if (status == Strophe.Status.DISCONNECTING) {
      log('Strophe is disconnecting.');
    }
    else if (status == Strophe.Status.DISCONNECTED) {
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
    vm_id = jmsg.find('text').text();
    console.log(vm_id);
    return true; // must have - if removed, removes the handler
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

