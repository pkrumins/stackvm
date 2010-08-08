function toImg (img64, imgType) {
    var imgTypes = {
        png : 'data:image/png;base64,',
        jpeg : 'data:image/jpeg;base64,',
        gif : 'data:image/gif;base64,'
    };

    if (!imgTypes[imgType])
        throw "Unknown imgType '" + imgType + "' was passed to toImg";

    // TODO: use MHTML for IE6
    return $('<img>').attr('src', imgTypes[imgType] + img64);
}

function CanvasDisplay () {
    var self = this;
    
    var canvas = $('<canvas>');
    self.element = $('<div>').addClass('canvasConsole');
    self.element.append(canvas);
    
    var canvasHTML = canvas[0];
    var context = canvasHTML.getContext('2d');

    self.resize = function (dims) {
        canvas.attr('width', dims.width);
        canvas.attr('height', dims.height);
        self.element.width(dims.width);
        self.element.height(dims.height);
    };
    
    self.rawRect = function (rect) {
        var img = toImg(rect.base64, rect.type);
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

    self.can = !!context;
}

function StackedDisplay () {
    var self = this;

    self.element = $('<div>').addClass('stackedConsole');

    self.rawRect = function (rect) {
        var img = toImg(rect.base64, rect.type);
        img.css({
            position : 'absolute',
            left : rect.x,
            top : rect.y,
            width : rect.width + 'px',
            height : rect.height + 'px',
        });
        self.element.append(img);
        //if (fullScreen) cleanupImages(img);
    }

    self.resize = function (dims) {
        self.element.width(dims.width);
        self.element.height(dims.height);
    };

    self.copyRect = function (rect) {
        console.log('got copyrect for stacked display');
    }

    function cleanupImages (except) {
        $('img', self.element)
            .not(except)
            .remove()
        ;
    }

    self.can = true;
}

function Display () {
    var display = new CanvasDisplay;
    if (!display.can)
        display = new StackedDisplay;

    this.element = display.element;
    this.rawRect = display.rawRect;
    this.resize = display.resize;
    this.copyRect = display.copyRect;
};

