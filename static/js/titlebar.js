TitleBar.prototype = new EventEmitter;
function TitleBar (params) {
    
    var self = this;
    var name = params.name;
    
    self.element = $('<div>')
        .addClass('title-bar')
        .append(
            $('<img>')
                .attr('src','/img/buttons/close.png')
                .addClass('window-button')
                .click(function () { self.emit('close') })
            ,
            $('<img>')
                .attr('src','/img/buttons/fullscreen.png')
                .addClass('window-button')
                .click(function () { self.emit('fullscreen') })
            ,
            $('<img>')
                .attr('src','/img/buttons/minimize.png')
                .addClass('window-button')
                .click(function () { self.emit('minimize') })
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
