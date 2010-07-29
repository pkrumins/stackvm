QuickBar.prototype = new EventEmitter;
function QuickBar (params) {
    var self = this;
    var vms = [];
    
    self.element = $('<div>')
        .attr('id','quick-bar')
    ;
    
    self.push = function (vm) {
        vms.push(vm);
        self.element.append(
            $('<div>').text(vm.name)
        );
    };
}
