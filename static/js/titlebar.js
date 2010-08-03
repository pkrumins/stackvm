TitleBar.prototype = new EventEmitter;
function TitleBar (params) {
    
    var self = this;
    var name = params.name;
    
    var menu = $('<div>')
        .addClass('window-menu')
        .hide()
        .append(
            $('<div>')
                .addClass('menu-item')
                .text('restart')
                .click(function () { self.emit('restart') })
            ,
            $('<div>')
                .addClass('menu-item')
                .text('kill')
                .click(function () { self.emit('kill') })
            ,
            $('<div>')
                .addClass('menu-item')
                .text('screenshot')
                .click(function () { self.emit('screenshot') })
            ,
            $('<div>')
                .addClass('menu-item')
                .text('start screencast')
                .click(function () { self.emit('startScreencast') })
        )
    ;
    
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
                .attr('src','/img/buttons/menu-down.png')
                .addClass('window-button')
                .addClass('menu-button')
                .toggle(
                    function () {
                        $(this).attr('src','/img/buttons/menu-up.png');
                        menu.fadeTo(250,0.95);
                    },
                    function () {
                        $(this).attr('src','/img/buttons/menu-down.png');
                        menu.fadeOut(250);
                    }
                )
            ,
            $('<div>').text(name),
            menu
        )
    ;
}
