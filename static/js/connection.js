var Connection = (function() {
    var connection = null;
    var reallyConnected = false;
    var msgQueue = [];
    var eventHandlers = {};

    function connected() {
        return reallyConnected;
    }
    
    // A browser might support a transport, but that doesn't mean it will work
    // Not using: flashsocket
    var transports = 'websocket htmlfile xhr-multipart xhr-polling'
        .split(/\s+/);
    var iv = false;
    
    function connect() {
        connection = new io.Socket(window.location.hostname, {
            rememberTransport : false,
            transports : transports,
            port : window.location.port
        });
        
        if (!iv) {
            // can't connect using this transport, try another
            iv = setInterval(function () {
                console.log('Transport ' + transports[0] + ' not working');
                transports.shift();
                if (transports.length == 0) {
                    clearInterval(iv);
                    throw 'No transports seem to work';
                }
            }, 2000);
        }
        
        connection.addEvent('connect', function() {
            clearInterval(iv);
            reallyConnected = true;
            processMsgQueue();
        });
        
        connection.addEvent('disconnect', function() {
            reallyConnected = false;
        });
        
        connection.addEvent('message', function(msg) {
            dispatchMsg(msg);
        });
        
        connection.connect();
    }

    function dispatchMsg(msg) {
        var msg = JSON.parse(msg);
        var handler = findEventHandler(msg.vmId);
        if (!handler) {
            console.log("Unknown VM '" + msg.vmId + "'");
            return;
        }
        actionfn = handler[msg.action];
        if (!actionfn) {
            console.log("Unknown action '" + msg.action + "' for VM '" + msg.vmId + "'");
            return;
        }
        actionfn(msg);
    }

    function processMsgQueue() {
        for (var i = 0; i < msgQueue.length; i++) {
            sendMsg(msgQueue[i]);
        }
        msgQueue = [];
    }

    function sendMsg(msg) {
        if (!connected()) {
            msgQueue.push(msg);
            connect();
        }
        else {
            connection.send(msg);
        }
    }

    function findEventHandler(vmId) {
        return eventHandlers[vmId];
    }

    return {
        addEventHandler: function(handler) {
            eventHandlers[handler.vm.vmId] = handler;
        },

        delEventHandler: function(vmId) {
            delete eventHandlers[vmId];
        },

        sendMsg: function(msg) {
            sendMsg(msg);
        }
    }
})();

exports.Connection = Connection;

