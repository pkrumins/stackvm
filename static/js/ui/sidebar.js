SideBar.prototype = new EventEmitter;
function SideBar (params) {
    var self = this;
    var engines = params.engines;
    
    var elements = {
        disks : $('<div>'),
        contacts : $('<div>'),
        screenshots : $('<div>'),
        screencasts : $('<div>'),
    };
    
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
                    menu.push('disk images', elements.disks);
                })
        ),
        $('<p>').append(
            $('<a>').text('screenshots')
                .click(function () {
                    menu.push('screenshots', elements.screenshots)
                })
        ),
        $('<p>').append(
            $('<a>').text('screencasts')
                .click(function () {
                    menu.push('screencasts', elements.screencasts)
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
        var elem = $('<div>')
            .addClass('contact')
            .addClass('contact-' + (contact.online ? 'online' : 'offline'))
            .text(contact.name)
            .click(function () {
                if ($(this).hasClass('contact-online')) {
                    self.emit('chat', contact);
                }
            })
            .appendTo(elements.contacts)
        ;
        contact.subscribe(function (sub) {
            sub.on('online', function () {
                elem.removeClass('contact-offline').addClass('contact-online');
            });
            sub.on('offline', function () {
                elem.removeClass('contact-online').addClass('contact-offline');
            });
        });
    };
    
    self.addDisk = function (disk) {
        var engineLinks = $('<span>');
        engines.forEach(function (engine) {
            engineLinks.append($('<a>')
                .text(engine)
                .click(function () { disk.spawn(engine) })
            , $('<span>').text(' '));
        });
        
        var ol = $('<ol>').addClass('instances').attr('start',0);
        
        var div = $('<div>')
            .addClass('disk')
            .text(disk.name)
            .append(
                $('<div>')
                    .addClass('disk-filename')
                    .text(disk.filename)
                ,
                $('<div>')
                    .addClass('disk-spawn')
                    .text('Spawn in ')
                    .append(engineLinks)
                ,
                ol
            )
        ;
        elements.disks.append(div);
        
        function addInstance (proc) {
            var elem = $('<li>')
                .data('addr', proc.addr)
                .text(proc.engine + ':' + proc.pid)
                .click(function () { self.emit('attach', proc) })
                .appendTo(ol)
            ;
            proc.subscribe(function (sub) {
                sub.on('exit', function () {
                    elem.remove();
                });
            });
        };
        
        function addScreenX(elem, url) {
            $('<p>').append($('<a>')
                .attr('href', url)
                .text(url.split('/').slice(-1)[0])
            ).appendTo(elem);
        }
        
        disk.subscribe(function (sub) {
            sub.on('spawn', addInstance);
            sub.on('screenshot', function (url) {
                addScreenX(elements.screenshots, url);
            });
            sub.on('screencast', function (url) {
                addScreenX(elements.screencasts, url);
            });
        });
        Hash(disk.processes).forEach(addInstance);
    };
}

