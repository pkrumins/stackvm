Workspace.prototype = new EventEmitter;
function Workspace (params) {
    if (!(this instanceof Workspace)) return new Workspace(params);
    var self = this;
    
    var dragging = false;
    var lastPos = null;
    
    self.element = $('<div>')
        .attr('id','workspace')
        .mousedown(function (ev) {
            if ($(ev.target).attr('id') == 'workspace') {
                dragging = true;
                ev.preventDefault();
            }
        })
        .mouseup(function (ev) {
            if ($(ev.target).attr('id') == 'workspace') {
                dragging = false;
                ev.preventDefault();
            }
        })
        .mouseleave(function (ev) { dragging = false })
        .mousemove(function (ev) {
            if (dragging && lastPos) {
                var delta = {
                    x : ev.pageX - lastPos.x,
                    y : ev.pageY - lastPos.y
                };
                windows.forEach(function (win) {
                    var pos = win.element.offset();
                    pos.left += delta.x;
                    pos.top += delta.y;
                    win.element.offset(pos);
                });
                ev.preventDefault();
            }
            lastPos = { x : ev.pageX, y : ev.pageY };
        })
    ;
    
    var windows = [];
    
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
        
        win.on('exit', function () {
            self.detachWindow(win);
        });
        
        self.element.append(win.element);
        
        windows.push(win);
    };
    
    self.detachWindow = function (win) {
        win.removeAllListeners();
        win.element.remove();
        var i = windows.indexOf(win);
        if (i >= 0) windows.splice(i,1);
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

