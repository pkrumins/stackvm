function Console(console_elem) {
    function get_screen() {
        var im = $(document.createElement("img"))
            .attr("src", "/api/console/get_screen"
                + "?" + String(Math.floor(Math.random() * 1e10))
            )
            .css({
                position : "absolute",
                left : 0, top : 0
            });
        console_elem
            .empty()
            .append(im);
    }
    
    function get_update(args) {
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
    
    function get_updates(version_id) {
        var uri = "/api/console/get_update_list/" + String(version_id)
            + "?" + String(Math.floor(Math.random() * 1e10));
        $.ajax({
            url : uri,
            dataType : "json",
            cache : false,
            success : function (data) {
                var latest_version = data[0][0];
                var update_count = data[0][1];
                if (update_count < 0) {
                    get_screen();
                }
                else {
                    for (var i = 1; i < data.length; i++) {
                        var item = data[i];
                        console.log(item.join(","));
                        get_update({
                            version_id : version_id,
                            update_id : i - 1,
                            x : item[0],
                            y : item[1],
                            width : item[2],
                            height : item[3]
                        });
                    }
                }
                get_updates(latest_version);
            },
            error : function () {
                get_updates(version_id);
            },
            timeout : 3000,
        });
    };
    
    this.run = function () {
        get_screen();
        get_updates(0);
    };
    
    var up = this;
}
