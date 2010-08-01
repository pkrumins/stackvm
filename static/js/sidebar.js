function SideMenu () {
    var self = this;
    
    var menuStack = [];
    self.element = $('<div>').attr('id','side-menu');
    
    self.push = function (title, item) {
        if (menuStack.length) {
            self.top().children('.side-menu-body').hide();
        }
        
        var index = menuStack.length;
        var elem = $('<div>')
            .addClass('side-menu')
            .append(
                $('<div>')
                    .addClass('side-menu-title')
                    .text(title)
                    .click(function () {
                        while (menuStack.length - 1 > index) self.pop();
                    })
                ,
                $('<div>').addClass('side-menu-body').append(item)
            )
        ;
        
        menuStack.push(elem);
        self.element.append(elem);
    };
    
    self.pop = function () {
       menuStack.pop().remove();
       if (menuStack.length) {
           self.top().children('.side-menu-body').show();
       }
    };
    
    self.top = function () {
        return menuStack.slice(-1)[0];
    };
}
    
SideBar.prototype = new EventEmitter;
function SideBar (params) {
    var self = this;
    var contacts = params.contacts;
    var instances = params.instances;
    
    var elements = {
        contacts : $('<div>'),
        instances : $('<div>')
    };
    
    contacts.on('list', function (people) {
        elements.contacts.empty();
        people.forEach(function (who) {
            elements.contacts.append($('<div>')
                .data('name', who.name)
                .text(who.name + ' [' + who.status + ']')
            );
        });
    });
    
    contacts.on('status', function (who) {
        elements.contacts.children('div').each(function () {
            if ($(this).data('name') == who.name) {
                $(this).text(who.name + ' [' + who.status + ']');
            }
        });
    });
    
    instances.on('list', function (vmHash) {
        elements.instances.empty();
        Object.keys(vmHash).forEach(function (id) {
            var vm = vmHash[id];
            var div = $('<ol>')
                .data('vm',vm.id)
                .text(vm.name + ':')
            ;
            elements.instances.append(div);
            vm.instances.forEach(function (inst) {
                div.append($('<li>')
                    .text(inst.engine)
                    .data('host',inst.host)
                    .click(function () { self.emit('attach', inst) })
                );
            });
        });
    });
    
    instances.on('spawn', function (vm,proc) {
        elements.instances.children('ol').filter(function (ol) {
            return ol.data('disk', vm.filename)
        }).append($('<li>')
            .text(proc.engine)
            .data('host', proc.host)
            .click(function () { self.emit('attach', proc.host) })
        );
    });
    
    instances.on('kill', function (host) {
        elements.instances.children('ol li').filter(function (li) {
            return li.data('host') == host
        }).remove();
    });
    
    var menu = new SideMenu;
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

