Window.prototype = new EventEmitter;
function Window (params) {
    var self = this;
    var remoteFB = params.remoteFB;
    var proc = params.proc;
    
    var fb = new FB(remoteFB);
    
    self.titleBar = new TitleBar({
        name : proc.disk,
        window : self
    });
    
    self.titleBar.on('kill', function () { proc.kill() });
    
    Window[params.host] = self;
    
    self.element = $('<div>')
        .append(self.titleBar.element.hide())
        .append(fb.element)
        .addClass('vm-window')
        .width(remoteFB.size.width)
        .height(remoteFB.size.height)
        .offset({ left : 100, top : 100 })
        .click(function (ev) { if (!focused) self.focus() })
        .draggable({
            handle : self.titleBar.element,
            stack : '.vm-window'
        });
    ;
    
    fb.on('resize', function (dims) {
        self.element
            .width(dims.width)
            .height(dims.height)
        ;
        self.titleBar.element.width(dims.width - 1);
    });
    
    var focused = true;
    
    self.focus = function () {
        if (!focused) {
            focused = true;
            self.titleBar.element.fadeIn(300);
            self.titleBar.element.width(fb.element.width() - 1);
            self.element.height(fb.element.height());
        }
        return self;
    };
    
    self.unfocus = function () {
        if (focused) {
            focused = false;
            self.emit('unfocus');
            fb.unfocus();
            self.element.removeClass('vm-window-focused');
            self.titleBar.element.width(fb.element.width() - 1);
            self.titleBar.element.fadeOut(300, function () {
                self.element.height(fb.element.height());
            });
        }
        return self;
    };
    
    self.unfocus();
}

