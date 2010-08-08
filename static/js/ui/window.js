Window.prototype = new EventEmitter;
function Window (params) {
    var self = this;
    var remoteFB = params.remoteFB;
    var proc = params.proc;
    self.addr = proc.addr;
    
    var fb = new FB(remoteFB);
    
    self.titleBar = new TitleBar(proc);
    
    if (!proc.shared) {
        self.titleBar.on('kill', function () { proc.kill() });
    }
    
    fb.on('end', function () { self.emit('exit') });
    
    self.element = $('<div>')
        .append(self.titleBar.element)
        .append(fb.element)
        .addClass('vm-window')
        .width(remoteFB.size.width)
        .height(remoteFB.size.height)
        .css('margin-bottom', -remoteFB.size.height - 6)
        .offset({
            left : 220 + ($(window).width() - 220 - remoteFB.size.width) / 2,
            top : Math.max(40, ($(window).height() - remoteFB.size.height) / 2)
        })
        .click(function (ev) { fb.focus() })
        .draggable({
            handle : self.titleBar.element,
            appendTo : 'body',
            stack : '.vm-window'
        });
    ;
    
    fb.on('resize', function (dims) {
        self.element
            .width(dims.width)
            .height(dims.height)
            .css('margin-bottom', -dims.height - 6)
        ;
        self.titleBar.element.width(dims.width - 1);
    });
}

