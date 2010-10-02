// A no-frills graphical container for the remote VM framebuffer
// Renders the framebuffer and handles the keyboard and mouse

FB.prototype = new EventEmitter;
function FB (remote) {
    var self = this;
    
    var input = remote.input;
    var encoder = remote.encoder;
    var size = remote.size;
    
    var mouseCoords = null;
    
    var focus = false;
    self.focus = function () {
        focus = true;
        self.element.focus();
    };
    
    self.unfocus = function () {
        focus = false;
        $(window).focus();
    };
    
    var mouseMask = 0;
    
    self.element = $('<div>')
        .addClass('fb')
        .attr('tabindex', 0) // so the div can receive focus
        .width(size.width)
        .height(size.height)
        .mousemove(function (ev) {
            self.focus();
            if (focus && input) {
                var pos = calcMousePos(ev);
                input.sendPointer(pos.x, pos.y, mouseMask);
            }
        })
        .mousedown(function (ev) {
            if (focus) mouseMask = 1;
            var pos = calcMousePos(ev);
            if (input) input.sendPointer(pos.x, pos.y, mouseMask);
            ev.preventDefault();
        })
        .mouseup(function (ev) {
            if (focus) mouseMask = 0;
            var pos = calcMousePos(ev);
            if (input) input.sendPointer(pos.x, pos.y, mouseMask);
            ev.preventDefault();
        })
        .mousewheel(function (ev, delta) {
            var pos = calcMousePos(ev);
            if (delta > 0 && input) { // mouse up
                input.sendPointer(pos.x, pos.y, 1 << 3);
                input.sendPointer(pos.x, pos.y, 0);
            }
            else if (input) {
                input.sendPointer(pos.x, pos.y, 1 << 4);
                input.sendPointer(pos.x, pos.y, 0);
            }
            ev.preventDefault();
        })
        // Other events should just call this element's key events when key
        // events occur elsewhere but this vm has focus
        .keydown(function (ev) {
            if (focus && input) {
                input.sendKeyDown(KeyMapper.getKeySym(ev.keyCode));
                ev.preventDefault();
            }
        })
        .keyup(function (ev) {
            if (focus && input) {
                input.sendKeyUp(KeyMapper.getKeySym(ev.keyCode));
                ev.preventDefault();
            }
        })
    ;
    
    function calcMousePos (ev) {
        var x = ev.pageX - self.element.offset().left;
        var y = ev.pageY - self.element.offset().top;
        return { x : x, y : y };
    }
    
    var display = new Display;
    display.resize(size);
    self.element.append(display.element);
    
    encoder.subscribe(function (sub) {
        sub.on('end', function () { self.emit('end') });
        
        sub.on('desktopSize', function (dims) {
            size = dims;
            self.emit('resize', dims);
            self.element
                .width(dims.width)
                .height(dims.height)
            ;
            display.resize(dims);
        });
        
        sub.on('screenUpdate', function (update) {
            display.rawRect(update);
        });
        
        sub.on('copyRect', function (rect) {
            display.copyRect(rect);
        });
    });
    
    encoder.requestRedraw();
}

