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

    D.win = D.title = D.con = null;
    D.width = opts.width || 400;
    D.height = opts.height || 200;

    this.createWindow = function () {
        var win = $('<div>')
            .addClass('window')
            .attr('tabindex', 0)
            .width(D.width)

        var title = D.createTitle();
        var con = D.createConsole();

        win.draggable({
                handle : title,
                stack : '.window'
        });

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

function CanvasDisplay (vm, opts) {
    Display.call(this, vm, opts);
    var D = this;

    this.createConsole = function () {
        var con = $('<div>')
            .addClass('canvasConsole')
            .width(D.width)
            .height(D.height);
        var canvas = $('<canvas>')
            .attr('width', D.width)
            .attr('height', D.height);
        var textArea = $('<div>')
            .addClass('centerMessage')
            .hide();
        con.append(canvas).append(textArea);
        D.canvas = canvas[0];
        D.canvasCtx = D.canvas.getContext('2d');
        return con;
    }

    this.conDraw = function (img64, imgType, x, y, width, height, fullScreen) {
        var img = toImg(img64, imgType);
        img.load(function () {
            if (height > D.canvas.height) {
                D.canvas.height = height;
                D.con.height(height);
            }
            if (width > D.canvas.width) {
                D.canvas.width = width;
                D.win.width(width);
            }
            D.canvasCtx.drawImage(img[0], x, y, width, height);
        });
    }

    this.resize = function (width, height) {
        D.canvas.width = width;
        D.win.width(width);
        D.canvas.height = height;
        D.con.height(height);
    }

    this.copyRect = function (srcX, srcY, dstX, dstY, width, height) {
        D.canvasCtx.drawImage(D.canvas,
            srcX, srcY, width, height,
            dstX, dstY, width, height);
    }
}

exports.StackedDisplay = StackedDisplay;
exports.CanvasDisplay = CanvasDisplay;

