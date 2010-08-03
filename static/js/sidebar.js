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
    var engines = params.engines;
    
    var elements = {
        contacts : $('<div>'),
        instances : $('<div>')
    };
    
    contacts.on('list', function (list) {
        elements.contacts.empty();
        list.forEach(function (who) {
            if (who.status == 'offline') {
                elements.contacts.append($('<div>')
                    .data('name', who.name)
                    .text(who.name + ' [' + who.status + ']')
                );
            }
            else {
                elements.contacts.append($('<div>')
                    .data('name', who.name)
                    .append($('<a>')
                        .text(who.name + ' [' + who.status + ']')
                        .click(function () {
                            new ChatWindow
                        })
                    )
                );
            }
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
            var ol = $('<ol>')
                .attr('start', 0)
                .data('id',id)
            ;
            var select = $('<select>');
            engines.forEach(function (engine) {
                select.append(
                    $('<option>').val(engine).text(engine)
                );
            });
            
            elements.instances.append($('<div>').append(
                $('<div>').text(vm.name),
                $('<form>').append(
                    select,
                    $('<input>')
                        .attr('type','button')
                        .val('spawn')
                        .click(function () {
                            self.emit('spawn', vm,
                                select.children('option:selected').val()
                            );
                        })
                ),
                ol
            ));
            
            vm.instances.forEach(function (inst) {
                ol.append($('<li>')
                    .text(inst.engine)
                    .data('host',inst.host)
                    .addClass('instance')
                    .click(function () { self.emit('attach', vm, inst.host) })
                );
            });
        });
    });
    
    instances.on('spawn', function (vm, proc) {
        elements.instances.children('div').children('ol').filter(function () {
            return $(this).data('id') == vm.id;
        }).append($('<li>')
            .text(proc.engine)
            .data('host', proc.host)
            .addClass('instance')
            .click(function () { self.emit('attach', vm, proc.host) })
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

