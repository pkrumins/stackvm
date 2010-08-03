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
    var screenshots = params.screenshots;
    var screencasts = params.screencasts;
    var engines = params.engines;
    
    var elements = {
        contacts : $('<div>'),
        instances : $('<div>'),
        screenshots : $('<div>'),
        screencasts : $('<div>')
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
            .click(function () { self.emit('attach', proc.host) })
        );
    });
    
    instances.on('kill', function (host) {
        elements.instances.children('ol li').filter(function (li) {
            return li.data('host') == host
        }).remove();
    });

    function newScreenShotCast (el, url) {
        el.append(
            $('<p>').append(
                $('<a>').attr('href', url).text(url.split('/').slice(-1)[0])
            )
        );
    }

    screenshots.on('new', function (url) {
        newScreenShotCast(elements.screenshots, url);
    });

    screencasts.on('new', function (url) {
            alert(url);
        newScreenShotCast(elements.screencasts, url);
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
        ),
        $('<p>').append(
            $('<a>').text('screenshots')
                .click(function () {
                    menu.push('screenshots', elements.screenshots);
                })
        ),
        $('<p>').append(
            $('<a>').text('screencasts')
                .click(function () {
                    menu.push('screencasts', elements.screencasts);
                })
        )
    ));

    self.element = $('<div>')
        .addClass('sidebar')
        .attr('id','sidebar')
        .append(menu.element)
    ;
}

