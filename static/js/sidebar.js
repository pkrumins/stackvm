SideBar.prototype = new EventEmitter;
function SideBar (params) {
    var self = this;
    
    self.element = $('<div>')
        .addClass('sidebar')
        .attr('id','sidebar')
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

