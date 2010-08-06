function MenuStack () {
    var self = this;
    
    var menuStack = [];
    self.element = $('<div>').attr('id','side-menu');
    
    self.push = function (title, item) {
        if (menuStack.length) {
            self.top().children('.side-menu-body').hide();
        }
        
        var index = menuStack.length;
        var elem = $('<div>')
            .addClass('side-menu')
            .append(
                $('<div>')
                    .addClass('side-menu-title')
                    .text(title)
                    .click(function () {
                        while (menuStack.length - 1 > index) self.pop();
                    })
                ,
                $('<div>').addClass('side-menu-body').append(item)
            )
        ;
        
        menuStack.push(elem);
        self.element.append(elem);
    };
    
    self.pop = function () {
       menuStack.pop().remove();
       if (menuStack.length) {
           self.top().children('.side-menu-body').show();
       }
    };
    
    self.top = function () {
        return menuStack.slice(-1)[0];
    };
}
