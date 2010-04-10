function Console(win) {
    this.render = function () {
        var im = $(document.createElement("img"))
            .attr("src", "/api/console/get_screen"
                + "?" + String(Math.floor(Math.random() * 1e10))
            )
            .css({
                position : "absolute",
                left : win.css("left") + 2,
                top : win.css("top") + title.css("height") + 4
            })
            .load(function () {
                elem.empty();
                elem.prepend(im);
            });
        elem.prepend(im);
    }
    
    this.render_update = function (args) {
        var uri = "/api/console/get_update/"
            + String(args.version_id) + "/" + String(args.update_id);
            + "?" + String(Math.floor(Math.random() * 1e10));
        var im = $(document.createElement("img"))
            .attr("src", uri)
            .css({
                position : "absolute",
                left : win.css("left") + 2 + args.x,
                top : win.css("top") + title.css("height") + 4 + args.y,
                width : args.width,
                height : args.height
            });
        win.width(Math.max(
            win.width(), args.width + 2
        ));
        win.height(Math.max(
            win.height(), args.height + title.height() + 2
        ));
        
        elem.append(im);
    }
    
    function handle_updates (version_id) {
        var uri = "/api/console/get_update_list/" + String(version_id);
        $.ajax({
            url : uri,
            dataType : "json",
            cache : false,
            success : function (data) {
                var latest_version = data[0][0];
                for (var i = 1; i < data.length; i++) {
                    var item = data[i];
                    up.render_update({
                        version_id : version_id,
                        update_id : i - 1,
                        x : item[0],
                        y : item[1],
                        width : item[2],
                        height : item[3]
                    });
                }
                handle_updates(latest_version);
            },
            error : function () {
                handle_updates(version_id);
            },
            timeout : 5000,
        });
    };
    
    function keymap(code) {
        var syms = {
            8 : 0xff00 + 8, // backspace
            13 : 0xff00 + 13, // return
            17 : 0xffe4, // left control
            18 : 0xff00 + 18, // left shift
            191 : 47
        };
        return syms[code] || code;
    }
    
    this.send_key_down = function (key_code) {
        var code = keymap(key_code);
        if (code) {
            console.log("key down " + String(code));
            $.get("/api/console/send_key_down/" + String(code));
        }
    };
    
    this.send_key_up = function (key_code) {
        var code = keymap(key_code);
        if (code) {
            $.get("/api/console/send_key_up/" + String(code));
        }
    };
    
    this.run = function () {
        this.render();
        handle_updates(0);
    };
    
    this.active = false;
    
    var up = this;
    var title = $(document.createElement("div"))
        . addClass("title")
        . text("window title");
    win.append(title);
    
    var elem = $(document.createElement("div"))
        . addClass("console");
    win.append(elem);
    
    win.mouseover(function (ev) {
        up.active = true;
        title.addClass("active-title");
    });
    
    win.mouseout(function (ev) {
        up.active = false;
        title.removeClass("active-title");
    });
    
    win.width(400);
    win.draggable();
}
