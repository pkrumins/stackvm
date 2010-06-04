function Display(vm, opts) {
    var D = this;

    D.win = D.title = D.con = null;
    D.width = opts.width || 400;
    D.height = opts.height || 200;

    this.createWindow = function () {
        var win = $('<div>')
            .addClass('window')
            .attr('tabindex', 0)
            .data('vmId', vm.vmId)
            .width(D.width + 'px')
            .height(D.height + 'px')
            .draggable();

        var title = D.createTitle();
        var con = D.createConsole();

        D.win = win;
        D.title = title;
        D.con = con;

        win.append(title).append(con);

        win.mouseover(function(ev) {
             vm.focus(win);
        });
        
        win.mouseout(function(ev) {
             vm.unfocus(win);
        });
        
        win.keydown(function(ev) {
             vm.eventEmitter.sendKeyDown(ev.keyCode);
             ev.preventDefault();
        });
        
        win.keyup(function(ev) {
             vm.eventEmitter.sendKeyUp(ev.keyCode);
             ev.preventDefault();
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
        

        return win;
    }

    this.createTitle = function () {
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
                            D.win.remove();
                        }
                    )
                )
            )
        ).append(
            $('<div>').addClass('clear')
        );
        return title;
    }

    this.createConsole = function () {
        var con = $('<div>').addClass('console');
        con.append(
             $('<div>')
                .addClass('centerMessage')
                .hide()
        );
        return con;
    }

    this.consoleMessage = function (msg) {
        var conMsgWin = $('.centerMessage', D.con);
        if (!msg) conMsgWin.hide()
        else conMsgWin.show().text(msg);
    }

    this.focus = function () {
        D.win.focus();
        $('.title', D.win).addClass("activeTitle");
    }

    this.unfocus = function () {
        $('.focusremover').focus();
        $('.title', D.win).removeClass("activeTitle");
    }

    this.resize = function (width, height) {
        D.win.width(width || D.width);
        D.win.height(height && height+22 || D.height);
    }

    this.conDraw = function (img, x, y, width, height, fullScreen) {
        throw "override Display.conDraw";
    }
}

function StackedDisplay (vm, opts) {
    Display.call(this, vm, opts);
    var D = this;

    this.conDraw = function(img, x, y, width, height, fullScreen) {
        if (height > D.win.height() + 22) D.win.height(height+22);
        if (width > D.win.width()) D.win.width(width);
        img.css({
             position : 'absolute',
             left : x,
             top : y,
             width : width,
             height : height
        });
        D.con.append(img);
        if (fullScreen) cleanupImages(img);
    }

    function cleanupImages (except) {
        $('img', D.con)
            .not(except)
            .remove();
    }
}

exports.StackedDisplay = StackedDisplay;

