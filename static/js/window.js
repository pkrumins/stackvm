Window.prototype = new EventEmitter;
function Window (params) {
    var self = this;
    var fb = params.fb;
    var titleBar = new TitleBar({
        name : params.name,
        window : self
    });
    
    titleBar.on('minimize', function () {
        self.element.remove();
        self.emit('minimize');
    });
    
    var prevCoords = null;
    titleBar.on('fullscreen', function () {
        self.emit('fullscreen');
        self.element.addClass('vm-window-fullscreen');
        prevCoords = self.element.offset();
        self.element.offset({
            left : ($(window).width() - self.element.width()) / 2,
            top : ($(window).height() - self.element.height()) / 2
        });
    });
    
    function close () {
        self.element.remove();
        self.emit('close');
    }
    titleBar.on('close', close);
    fb.on('close', close);
    
    titleBar.on('kill', function () { self.emit('kill') });
    titleBar.on('restart', function () { self.emit('restart') });
    
    var focused = true;
    
    self.element = $('<div>')
        .hide()
        .append(titleBar.element.hide())
        .append(fb.element)
        .addClass('vm-window')
        .offset({ left : 100, top : 100 })
        .click(function (ev) {
            if (!focused) {
                self.focus();
            }
        })
        .draggable({
            handle : titleBar.element,
            stack : '.vm-window'
        });
    ;
    
    fb.on('resize', function (dims) {
        self.element
            .width(dims.width)
            .height(dims.height)
            .fadeIn(400)
        ;
        titleBar.element.width(dims.width - 1);
    });
    
    self.focus = function () {
        if (!focused) {
            focused = true;
            self.emit('focus');
            fb.focus();
            self.element.addClass('vm-window-focused');
            titleBar.element.fadeIn(300);
            titleBar.element.width(fb.element.width() - 1);
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
            titleBar.element.width(fb.element.width() - 1);
            titleBar.element.fadeOut(300, function () {
                self.element.height(fb.element.height());
            });
        }
        return self;
    };
    
    self.unfocus();
    
    self.restore = function () {
        self.element.removeClass('vm-window-fullscreen');
        self.element.offset(prevCoords);
    };
}

