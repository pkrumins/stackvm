// It's annoying having to work out the client-side library dependencies by
// hand and to update the HTML parts in order to use new libraries.

function require (src) {
    document.write(
        '<script src="/js/' + escape(src) + '" type="text/javascript"></script>'
    );
}

