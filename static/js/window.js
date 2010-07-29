Window.prototype = new EventEmitter;
function Window (params) {
    var self = this;
    var fb = params.fb;
    var tabBar = new TabBar({
        name : params.name,
        window : self
    });
    
    tabBar.on('minimize', function () {
        self.element.remove();
        self.emit('minimize');
    });
    
    tabBar.on('fullscreen', function () {
        self.emit('fullscreen');
        self.element.addClass('vm-window-fullscreen');
        self.element.offset({
            left : ($(window).width() - $(self.element).width()) / 2,
            top : ($(window).height() - $(self.element).height()) / 2
        });
    });
    
    tabBar.on('close', function () {
        self.element.remove();
        self.emit('close');
    });
    
    var focused = true;
    
    self.element = $('<div>')
        .append(tabBar.element.hide())
        .append(fb.element)
        .addClass('vm-window')
        .offset({ left : 100, top : 100 })
        .click(function (ev) {
            if (!focused) {
                self.focus();
            }
        })
        .draggable()
    ;
    
    fb.on('resize', function (dims) {
        self.element.width(dims.width);
        self.element.height(dims.height);
        tabBar.element.width(dims.width - 1);
    });
    
    self.focus = function () {
        if (!focused) {
            focused = true;
            self.emit('focus');
            fb.focus();
            self.element.addClass('vm-window-focused');
            tabBar.element.fadeIn(300);
            tabBar.element.width(fb.element.width() - 1);
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
            tabBar.element.width(fb.element.width() - 1);
            tabBar.element.fadeOut(300, function () {
                self.element.height(fb.element.height());
            });
        }
        return self;
    };
    
    self.unfocus();
}

