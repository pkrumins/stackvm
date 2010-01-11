function Console(console_elem) {
    function get_screen() {
        console_elem
            .empty()
            .append(
                $(document.createElement("img"))
                    .attr("src", "/api/console/get_screen")
                    .css({
                        position : "absolute",
                        left : 0, top : 0
                    })
            );
    }
    
    function get_update(args) {
        var uri = "/api/console/get_update/"
            + String(args.version_id) + "/" + String(args.update_id);
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
        var uri = "/api/console/get_update_list/" + String(version_id);
        $.getJSON(uri, function (data) {
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
            setTimeout(function () { get_updates(latest_version) }, 500);
        });
    };
    
    this.run = function () {
        get_screen();
        get_updates(0);
    };
    
    var up = this;
}
