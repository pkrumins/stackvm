Window.prototype = new EventEmitter;
function Window (params) {
    var self = this;
    var remoteFB = params.remoteFB;
    var proc = params.proc;
    
    var fb = new FB(remoteFB);
    
    self.titleBar = new TitleBar(proc);
    
    if (!proc.shared) {
        self.titleBar.on('kill', function () { proc.kill() });
    }
    
    Window[proc.addr] = self;
    
    self.element = $('<div>')
        .append(self.titleBar.element)
        .append(fb.element)
        .addClass('vm-window')
        .width(remoteFB.size.width)
        .height(remoteFB.size.height)
        .offset({ left : 100, top : 100 })
        .click(function (ev) { fb.focus() })
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
}

