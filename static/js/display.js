function toImg (img64, imgType) {
    var imgTypes = {
        png : 'data:image/png;base64,',
        jpeg : 'data:image/jpeg;base64,',
    };

    if (!imgTypes[imgType])
        throw "Unknown imgType '" + imgType + "' was passed to toImg";

    // TODO: use MHTML for IE6
    return $('<img>').attr('src', imgTypes[imgType] + img64);
}

function Display(vm, opts) {
    var D = this;

    D.win = D.title = D.con = D.infoStrip = null;
    D.width = opts.width || 400;
    D.height = opts.height || 200;

    this.createWindow = function () {
        var win = $('<div>')
            .addClass('window')
            .attr('tabindex', 0)
            .width(D.width)

        var title = D.createTitle();
        var infoStrip = D.createInfoStrip();
        var con = D.createConsole();

        win.draggable({
                handle : title,
                stack : '.window'
        });

        D.win = win;
        D.title = title;
        D.infoStrip = infoStrip;
        D.con = con;

        win.append(title).append(infoStrip).append(con);

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

        function calcMousePos (ev) {
            var x = ev.pageX - con.offset().left;
            var y = ev.pageY - con.offset().top;
            return {x:x, y:y};
        }
        
        con.mousemove(function(ev) {
            var pos = calcMousePos(ev);
            vm.eventEmitter.sendPointer(pos.x, pos.y, mouseMask);
        });
        
        con.mousedown(function(ev) {
            mouseMask = 1;
            var pos = calcMousePos(ev);
            vm.eventEmitter.sendPointer(pos.x, pos.y, mouseMask);
        });
        
        con.mouseup(function(ev) {
            mouseMask = 0;
            var pos = calcMousePos(ev);
            vm.eventEmitter.sendPointer(pos.x, pos.y, mouseMask);
        });

        con.mousewheel(function (ev, delta) {
            var pos = calcMousePos(ev);
            if (delta > 0) { // mouse up
                vm.eventEmitter.sendPointer(pos.x, pos.y, 1<<4);
                vm.eventEmitter.sendPointer(pos.x, pos.y, 0);
            }
            else {
                vm.eventEmitter.sendPointer(pos.x, pos.y, 1<<5);
                vm.eventEmitter.sendPointer(pos.x, pos.y, 0);
            }
        });

        return win;
    }

    this.createTitle = function () {
        var title = $('<div>').addClass('title');
        title.append($('<div>').addClass('text').text(vm.vmId));
        title.append(
            $('<div>')
            .addClass('buttons')
            .append(
                $('<span>').addClass('screenshot').append(
                    $('<img>')
                    .attr('src', '/img/screenshot.png')
                    .attr('title', 'Take a screenshot')
                    .click(function (ev) {
                        vm.eventEmitter.takeScreenshot();
                    })
                )
            )
            .append(
                $('<span>').addClass('screencast').append(
                    $('<img>')
                    .attr('src', '/img/screencast_start.png')
                    .attr('title', 'Record a screencast')
                    .click(function (ev) {
                        if (/start/.test($(this).attr('src'))) {
                            $(this)
                                .attr('src', '/img/screencast_stop.png')
                                .attr('title', 'Stop recording');
                            vm.eventEmitter.startScreencast();
                        }
                        else {
                            $(this)
                                .attr('src', '/img/screencast_start.png')
                                .attr('title', 'Record a screencast');
                            vm.eventEmitter.stopScreencast();
                        }
                    })
                )
            )
            .append(
                $('<span>').addClass('refresh').append(
                    $('<img>')
                    .attr('src', '/img/refresh.png')
                    .attr('title', 'Refresh screen')
                    .click(function (ev) {
                        vm.eventEmitter.redrawScreen();
                    })
                )
            )
            .append(
                $('<span>').addClass('close').append(
                    $('<img>')
                    .attr('src', '/img/close.png')
                    .attr('title', 'Disconnect')
                    .click(function (ev) {
                        vm.eventEmitter.detachVm();
                        VM.Manager.del(vm.vmId);
                        D.win.remove();
                    })
                )
            )
        ).append(
            $('<div>').addClass('clear')
        );
        return title;
    }

    this.createInfoStrip = function () {
        var strip = $('<div>')
            .addClass('infoStrip')
            .hide();
        strip.append(
            $('<div>').addClass('text')
        ).append(
            $('<div>').addClass('menu').append(
                $('<img>')
                .attr('src', '/img/close-infostrip.gif')
                .click(function (ev) {
                    strip.slideUp();
                })
            )
        ).append(
            $('<div>').addClass('clear')
        );
        return strip;
    }

    this.infoMessage = function (msg) {
        D.infoStrip.slideDown().html(msg);
    }

    this.createConsole = function () {
        var con = $('<div>')
            .addClass('stackedConsole')
            .width(D.width)
            .height(D.height)
            .append(
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
        D.con.width(width);
        D.win.width(width);
        D.con.height(height); // win height is determined by con height
    }

    this.conDraw = function (img64, imgType, x, y, width, height, fullScreen) {
        throw "override Display.conDraw";
    }

    this.copyRect = function (srcX, srcY, dstX, dstY, width, height) {
        throw "override Display.copyRect";
    }
}

function StackedDisplay (vm, opts) {
    Display.call(this, vm, opts);
    var D = this;

    this.conDraw = function(img64, imgType, x, y, width, height, fullScreen) {
        if (width > D.con.width()) {
            D.con.width(width);
            D.win.width(width);
        }
        if (height > D.con.height()) D.con.height(height);
        var img = toImg(img64, imgType);
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

    this.copyRect = function (srcX, srcY, dstX, dstY, width, height) {
        console.log('got copyrect for stacked display');
    }
}

function CanvasDisplay () {
    // only this display is updated for the dnode stuff so far
    var self = this;
    
    self.element = $('<div>').addClass('canvasConsole');
    
    var canvas = $('<canvas>');
    
    var textArea = $('<div>')
        .addClass('centerMessage')
        .hide()
    ;
    
    self.element.append(canvas).append(textArea);
    
    var canvasHTML = canvas.get(0);
    var context = canvasHTML.getContext('2d');
    
    self.resize = function (dims) {
        canvas.width(dims.width);
        canvas.height(dims.height);
        self.element.width(dims.width);
        self.element.height(dims.height);
    };
    
    self.rawRect = function (rect) {
        var img = toImg(img.base64, img.type);
        img.load(function () {
            context.drawImage(img[0], rect.x, rect.y, rect.width, rect.height);
        });
    };
    
    self.copyRect = function (rect) {
        context.drawImage(
            canvasHTML,
            rect.srcX, rect.srcY, rect.width, rect.height,
            rect.x, rect.y, rect.width, rect.height
        );
    };
}

