SideBar.prototype = new EventEmitter;
function SideBar (params) {
    var self = this;
    
    var menu = new MenuStack;
    menu.push('main menu', $('<div>').append(
        $('<p>').append(
            $('<a>').text('contacts')
                .click(function () {
                    menu.push('contacts', elements.contacts);
                })
        ),
        $('<p>').append(
            $('<a>').text('disk images')
                .click(function () {
                    menu.push('disk images', elements.instances);
                })
        )
    ));
    
    self.element = $('<div>')
        .addClass('sidebar')
        .attr('id','sidebar')
        .append(menu.element)
    ;
}

