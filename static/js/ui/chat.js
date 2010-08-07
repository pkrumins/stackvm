ChatWindow.prototype = new EventEmitter;
function ChatWindow (me, contact) {
    if (!(this instanceof ChatWindow)) return new ChatWindow();
    var self = this;
    
    self.contact = contact;
    
    var body = $('<div>')
        .addClass('chat-body')
        .droppable({
            //accept : '.title-text-drag',
            drop : function (ev, ui) {
                console.log($(this).data('share'));
                var fb = $(ui.draggable).data('share');
                console.dir(fb);
            }
        })
    ;
    
    self.element = $('<div>')
        .addClass('chat-window')
        .append(
            $('<div>')
                .addClass('chat-title')
                .text(contact.name)
                .append($('<div>')
                    .addClass('chat-x')
                    .text('[x]')
                    .click(function () {
                        self.emit('close');
                    })
                )
            ,
            body,
            $('<form>')
                .append(
                    $('<input>')
                        .attr('name', 'msg')
                )
                .submit(function (ev) {
                    ev.preventDefault();
                    var msg = $(this.elements.msg).val();
                    self.addMessage(me, msg);
                    contact.message(msg);
                    $(this.elements.msg).val('');
                })
        )
    ;
    
    self.addMessage = function (who, msg) {
        body.append($('<p>').append(
            $('<span>').addClass('chat-who').text(who),
            $('<span>').addClass('chat-msg').text(msg)
        ));
        $('.chat-body').animate({
            scrollTop : $('.chat-body').attr('scrollHeight')
        }, 500);
    };
    
    self.say = function (msg) {
        self.addMessage(me, msg);
        contact.message(msg);
    };
}

