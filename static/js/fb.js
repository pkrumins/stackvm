// A no-frills graphical container for the remote VM framebuffer
// Renders the framebuffer and handles the keyboard and mouse

function FB (params) {
    var self = this;
    var vm = params.vm; // the remote fb object served via dnode
    
    self.element = $('<div>').addClass('fb');
    
    vm.dimensions(function (dims) {
        self.element
            .width(dims.width)
            .height(dims.height)
        ;
    });
}

