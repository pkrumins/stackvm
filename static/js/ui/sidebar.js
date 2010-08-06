SideBar.prototype = new EventEmitter;
function SideBar (params) {
    var self = this;
    
    var disks = $('<div>');
    var contacts = $('<div>');
    
    var menu = new MenuStack;
    menu.push('main menu', $('<div>').append(
        $('<p>').append(
            $('<a>').text('contacts')
                .click(function () {
                    menu.push('contacts', contacts);
                })
        ),
        $('<p>').append(
            $('<a>').text('disk images')
                .click(function () {
                    menu.push('disk images', disks);
                })
        )
    ));
    
    self.element = $('<div>').addClass('sidebar').append(
        $('<img>')
            .attr('id', 'logo')
            .width(200).height(48)
            .attr('src', '/img/stackvm-200x48.png')
            .toggle(
                function () { $('.sidebar-menu').fadeOut(400) },
                function () { $('.sidebar-menu').fadeIn(400) }
            )
        ,
        $('<div>').addClass('sidebar-menu').append(menu.element)
    );
    
    self.addContact = function (contact) {
        contacts.append($('<div>')
            .addClass('contact')
            .addClass('contact-' + (contact.online ? 'online' : 'offline'))
            .text(contact.name)
        );
    };
    
    self.updateContact = function (name, status) {
        $('.contact').each(function () {
            if ($(this).text() == name) {
                $(this)
                    .removeClass('contact-online')
                    .removeClass('contact-offline')
                    .addClass('contact-' + status)
                ;
            }
        });
    };
    
    self.addDisk = function (disk) {
        disks.append($('<div>')
            .addClass('disk')
            .text(disk.name)
            .append($('<div>')
                .addClass('disk-filename')
                .text(disk.filename)
            )
        );
    };
}

