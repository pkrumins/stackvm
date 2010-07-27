// A no-frills graphical container for the remote VM framebuffer
// Renders the framebuffer and handles the keyboard and mouse

FB.prototype = new EventEmitter;
function FB (params) {
    var self = this;
    var vm = params.vm; // the remote fb object served via dnode
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
        .mousemove(function (ev) {
            self.focus();
            if (focus) {
                var pos = calcMousePos(ev);
                vm.sendPointer(pos.x, pos.y, mouseMask);
            }
        })
        .mousedown(function (ev) {
            if (focus) mouseMask = 1;
        })
        .mouseup(function (ev) {
            if (focus) mouseMask = 0;
        })
        .mousewheel(function (ev, delta) {
            var pos = calcMousePos(ev);
            if (delta > 0) { // mouse up
                vm.sendPointer(pos.x, pos.y, 1 << 4);
                vm.sendPointer(pos.x, pos.y, 0);
            }
            else {
                vm.sendPointer(pos.x, pos.y, 1 << 5);
                vm.sendPointer(pos.x, pos.y, 0);
            }
        })
        // Other events should just call this element's key events when key
        // events occur elsewhere but this vm has focus
        .keydown(function (ev) {
            if (focus) {
                vm.sendKeyDown(KeyMapper.getKeySym(ev.keyCode));
                ev.preventDefault();
            }
        })
        .keyup(function (ev) {
            if (focus) {
                vm.sendKeyUp(KeyMapper.getKeySym(ev.keyCode));
                ev.preventDefault();
            }
        })
    ;
    
    function calcMousePos (ev) {
        var x = ev.pageX - self.element.offset().left;
        var y = ev.pageY - self.element.offset().top;
        return { x : x, y : y };
    }
    
    var display = new CanvasDisplay;
    self.element.append(display.element);
    
    function desktopSize (dims) {
        self.element
            .width(dims.width)
            .height(dims.height)
        ;
        display.resize(dims);
        self.emit('resize', dims);
    }
    vm.on('desktopSize', desktopSize);
    vm.dimensions(desktopSize);
    
    var firstRect; firstRect = function () {
        vm.requestRedrawScreen();
        firstRect = function () {};
    };
    setTimeout(firstRect, 500);
    
    vm.on('png', function (png) {
        firstRect();
        png.type = 'png';
        display.rawRect(png);
    });
    
    vm.on('copyRect', function (rect) {
        firstRect();
        display.copyRect(rect);
    });
}

