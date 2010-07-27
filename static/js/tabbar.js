TabBar.prototype = new EventEmitter;
function TabBar (params) {
    var self = this;
    var name = params.name;
    
    self.element = $('<div>')
        .addClass('tab-bar')
        .append(
            $('<img>')
                .attr('src','/img/buttons/close.png')
                .addClass('window-button')
            ,
            $('<img>')
                .attr('src','/img/buttons/fullscreen.png')
                .addClass('window-button')
            ,
            $('<img>')
                .attr('src','/img/buttons/minimize.png')
                .addClass('window-button')
                .click(function () { self.emit('detach') })
            ,
            $('<img>')
                .attr('src','/img/buttons/menu.png')
                .addClass('window-button')
                .attr('id','menu-button')
            ,
            $('<div>').text(name)
        )
    ;
}
