QuickBar.prototype = new EventEmitter;
function QuickBar (params) {
    var self = this;
    var vms = [];
    
    self.element = $('<div>')
        .attr('id','quick-bar')
    ;
    
    self.push = function (vm, host) {
        vms.push(vm);
        var div = $('<div>')
            .append(
                $('<img>')
                    .attr('src', '/img/icons/tux.png')
                    .attr('width', 64)
                    .attr('height', 75)
                ,
                $('<div>').text(vm.name)
            )
            .click(function () {
                self.emit('restore', vm, host);
                div.fadeOut(400, function () { div.remove() });
            })
        ;
        self.element.append(div);
    };
}

