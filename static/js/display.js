function SimpleDisplay (vm, opts) {
    var D = this;

    D.width = (opts.width || 400) + 4; // + 4 because of 2x2px padding
    D.height = opts.height || 200;

    this.createWindow = function () {
        var win = $('<div>')
            .addClass('window')
            .attr('tabindex', 0)
            .data('vmId', vm.vmId)
            .width(String(D.width) + 'px')
            .height(String(D.height) + 'px')
            .draggable();

        var title = $('<div>').addClass('title');
        title.append($('<div>').addClass('text').text(vm.vmId));
        title.append(
            $('<div>').addClass('buttons').append(
                $('<span>').addClass('refresh').append(
                    $('<img>').attr('src', '/img/refresh.png').click(
                        function (ev) {
                            vm.eventEmitter.redrawScreen();
                        }
                    )
                )
            ).append(
                $('<span>').addClass('close').append(
                    $('<img>').attr('src', '/img/close.png').click(
                        function (ev) {
                            vm.eventEmitter.detachVm();
                            VM.Manager.del(vm.vmId);
                            win.remove();
                        }
                    )
                )
            )
        ).append(
            $('<div>').addClass('clear')
        );
        win.append(title);

        var con = $('<div>').addClass('console');
        con.append(
             $('<div>')
                . addClass('centerMessage')
                . text('Loading ' + vm.vmId + '...')
        );
        win.append(con);
        win.mouseover(function(ev) {
             vm.focus(win);
        });
        
        win.mouseout(function(ev) {
             vm.unfocus(win);
        });
        
        var mouseMask = 0;
        
        con.mousemove(function(ev) {
             var x = ev.pageX - con.offset().left;
             var y = ev.pageY - con.offset().top;
             vm.eventEmitter.sendPointer(x, y, mouseMask);
        });
        
        con.mousedown(function(ev) {
             mouseMask = 1;
             vm.eventEmitter.sendPointer(ev.pageX, ev.pageY, mouseMask);
        });
        
        con.mouseup(function(ev) {
             mouseMask = 0;
             vm.eventEmitter.sendPointer(ev.pageX, ev.pageY, mouseMask);
        });
        
        win.keydown(function(ev) {
             vm.eventEmitter.sendKeyDown(ev.keyCode);
             ev.preventDefault();
        });
        
        win.keyup(function(ev) {
             vm.eventEmitter.sendKeyUp(ev.keyCode);
             ev.preventDefault();
        });

        return win;
    }
}

exports.SimpleDisplay = SimpleDisplay;

