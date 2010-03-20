function Console(console_elem) {
    this.render = function () {
        var im = $(document.createElement("img"))
            .attr("src", "/api/console/get_screen"
                + "?" + String(Math.floor(Math.random() * 1e10))
            )
            .css({
                position : "absolute",
                left : 0, top : 0
            })
            .load(function () {
                console_elem.empty();
                console_elem.prepend(im);
            });
        console_elem.prepend(im);
    }
    
    this.render_update = function (args) {
        var uri = "/api/console/get_update/"
            + String(args.version_id) + "/" + String(args.update_id);
            + "?" + String(Math.floor(Math.random() * 1e10));
        var im = $(document.createElement("img"))
            .attr("src", uri)
            .css({
                position : "absolute",
                left : args.x,
                top : args.y,
                width : args.width,
                height : args.height
            });
        console_elem.append(im);
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
    
    var up = this;
}
