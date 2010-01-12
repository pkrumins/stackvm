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
    
    this.get_latest_version = function () {
        var version;
        $.ajax({
            async : false,
            url : "/api/console/get_latest_version",
            dataType : "json",
            cache : false,
            success : function (data) { version = data },
            error : function () { version = 0 },
            timeout : 3000
        });
        return version;
    }
    
    function handle_updates (version_id) {
        var uri = "/api/console/get_update_list/" + String(version_id);
        $.ajax({
            url : uri,
            dataType : "json",
            cache : false,
            success : function (data) {
                var latest_version = data[0][0];
                var update_count = data[0][1];
                if (update_count < 0) {
                    up.render();
                }
                else {
                    for (var i = 1; i < data.length; i++) {
                        var item = data[i];
                        console.log(item.join(","));
                        up.render_update({
                            version_id : version_id,
                            update_id : i - 1,
                            x : item[0],
                            y : item[1],
                            width : item[2],
                            height : item[3]
                        });
                    }
                }
                handle_updates(latest_version);
            },
            error : function () {
                handle_updates(version_id);
            },
            timeout : 3000,
        });
    };
    
    this.run = function () {
        var v = this.get_latest_version();
        this.render();
        handle_updates(v);
    };
    
    var up = this;
}
