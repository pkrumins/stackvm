// A no-frills graphical container for the remote VM framebuffer
// Renders the framebuffer and handles the keyboard and mouse

function FB (params) {
    var self = this;
    var vm = params.vm; // the remote fb object served via dnode
    
    self.element = $('<div>').addClass('fb');
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
    
    vm.addListener('copyRect', display.copyRect);
    
    vm.requestRedrawScreen();
}

