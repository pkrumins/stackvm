// A no-frills graphical container for the remote VM framebuffer
// Renders the framebuffer and handles the keyboard and mouse

function FB (params) {
    var self = this;
    var vm = params.vm; // the remote vm object served via dnode
    var width = params.width || 640;
    var height = params.height || 480;
    
    self.element = $('<div>')
        .width(width)
        .height(height)
        .addClass('fb')
    ;
}

