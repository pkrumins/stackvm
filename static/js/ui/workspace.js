Workspace.prototype = new EventEmitter;
function Workspace (params) {
    if (!(this instanceof Workspace)) return new Workspace(params);
    var self = this;
    
    self.element = $('<div>')
        .attr('id','workspace')
    ;
    
    self.attachWindow = function (win) {
        win.titleBar.on('minimize', function () {
            // ..
        });
        
        win.titleBar.on('fullscreen', function () {
            // ..
        });
        
        win.titleBar.on('close', function () {
            self.detachWindow(win);
        });
        
        win.titleBar.on('kill', function () {
            self.detachWindow(win);
        });
        
        win.titleBar.on('restart', function () {
            // ..
        });
        
        self.element.append(win.element);
    };
    
    self.detachWindow = function (win) {
        win.removeAllListeners();
        win.element.remove();
    };
    
    var chats = {};
    
    self.hasChat = function (name) {
        return name in chats
    };
    
    self.addChat = function (chat) {
        self.element.append(chat.element);
        
        var rightMost = Math.min.apply({}, [$(window).width()].concat(
            Object.keys(chats).map(function (name) {
                return chats[name].element.offset().left;
            })
        ));
        chat.element.css('right', $(window).width() - rightMost + 10);
        
        chats[chat.contact.name] = chat;
        
        chat.on('close', function () {
            chat.element.remove();
            delete chats[chat.contact.name];
        });
        
        chat.on('attach', function (vm) {
            self.emit('attach', vm);
        });
    };
    
    self.routeChat = function (msg) {
        chats[msg.from.name].addMessage(msg.from.name, msg.message);
    };
    
    self.routeResource = function (vm) {
        chats[vm.from.name].addResource(vm);
    };
}

