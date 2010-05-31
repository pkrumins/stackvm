var require = (function () {

var required = {};

function require (src) {
    if (required[src]) return required[src];

    var exports = {};
    (function () {
        eval(getSync('/js/' + src + '.js'));
    }).call(exports);
    required[src] = exports;
    return exports;

    function getSync(uri) {
        var http = false;
        if (window.XMLHttpRequest) {
            http = new XMLHttpRequest();
        }
        else {
            http = new ActiveXObject("Microsoft.XMLHTTP");
        }
        if (http) {
            http.open('GET', uri, false); // false makes it synchronous
            http.send();
            return http.responseText;
        }
    }
}

return require;

})();

