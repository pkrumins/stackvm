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
    
    var menu = new SideMenu;
    menu.push('main menu', $('<div>').append(
        $('<p>').text('main stuff here!'),
        $('<a>').text('contacts')
            .click(function () {
                menu.push('contacts', 'contacts stuff');
            })
    ));
    
    self.element = $('<div>')
        .addClass('sidebar')
        .attr('id','sidebar')
        .append(menu.element)
    ;
    
    /*
    var settingsBox = $('<div>').addClass('settings-box');
    leftPane.append(settingsBox);
    
    var chatBox = $('<div>')
        .attr('id','chat-box')
        .append(
            $('<div>').text('Contacts'),
            $('<p>').text('meow')
        )
    ;
    leftPane.append(chatBox);
    
    var infoPanes = {};
    self.addInfoPane = function (vm) {
        var elem = $('<div>')
            .hide()
            .addClass('info-pane')
            .append(
                $('<div>')
                    .addClass('back')
                    .text('back')
                    .click(function () {
                        settingsBox.hide();
                        vmPane.fadeIn(400);
                        elem.fadeOut(400);
                    })
                ,
                $('<p>').text(vm.name),
                $('<p>').text('Instances:'),
                $('<p>').append.apply(
                    $('<p>').addClass('instance-list'),
                    vm.processes.map(function (proc,i) {
                        return $('<p>').append(
                            $('<span>').text('[' + i + '] '),
                            $('<a>')
                                .data('host', proc.host)
                                .text(proc.engine)
                                .click(function () {
                                    self.attach(vm, proc.host);
                                })
                        );
                    })
                ),
                $('<a>')
                    .text('spawn in ' + vm.engine)
                    .click(function () {
                        self.spawn(vm, vm.engine);
                    })
            )
        ;
        infoPanes[vm.id] = elem;
        leftPane.append(elem);
    };
    */
}

