// A no-frills graphical container for the remote VM framebuffer
// Renders the framebuffer and handles the keyboard and mouse

function FB (params) {
    var self = this;
    var vm = params.vm; // the remote fb object served via dnode
    var mouseCoords = null;
    
    var focus = true;
    self.focus = function () { focus = true };
    self.unfocus = function () { focus = false };
    
    var mouseMask = 0;
    
    self.element = $('<div>')
        .addClass('fb')
        .mousemove(function (ev) {
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
    }
    vm.dimensions(desktopSize);
    vm.addListener('desktopSize', desktopSize);
    
    vm.addListener('png', function (png) {
        png.type = 'png';
        display.rawRect(png);
    });
    
    vm.addListener('copyRect', function (rect) {
        display.copyRect(rect);
    });
    
    vm.requestRedrawScreen();
}

