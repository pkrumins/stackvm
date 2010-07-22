function TabBar (params) {
    var self = this;
    var name = params.name;
    
    self.element = $('<div>')
        .addClass('tab-bar')
        .append($('<div>').text(name))
    ;
}
