TitleBar.prototype = new EventEmitter;
function TitleBar (proc) {
    
    var self = this;
    
    var menu = $('<div>')
        .addClass('window-menu')
        .hide()
        .append(
            $('<div>')
                .addClass('menu-item')
                .text('restart')
                .click(function () {
                    $('.menu-button').click();
                    self.emit('restart');
                })
            ,
            $('<div>')
                .addClass('menu-item')
                .text('kill')
                .click(function () {
                    $('.menu-button').click();
                    self.emit('kill');
                })
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
            $('<div>')
                .addClass('title-text')
                .text(proc.name)
                .draggable({
                    appendTo : 'body',
                    scroll : false,
                    helper : function () {
                        return $('<div>')
                            .text(proc.name)
                            .addClass('title-text-drag')
                        ;
                    }
                })
                .data('proc', proc)
            ,
            menu
        )
    ;
}

