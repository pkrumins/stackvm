ChatWindow.prototype = new EventEmitter;
function ChatWindow (params) {
    if (!(this instanceof ChatWindow)) return new ChatWindow();
    var self = this;
    
    var me = params.me;
    self.contact = params.contact;
    
    var body = $('<div>')
        .addClass('chat-body')
        .droppable({
            //accept : '.title-text-drag',
            drop : function (ev, ui) {
                var proc = $(ui.draggable).data('proc');
                body.append($('<div>')
                    .addClass('chat-share')
                    .append(
                        $('<span>').text(proc.name),
                        $('<a>')
                            .text('[share]')
                            .click(function () {
                                self.contact.share(proc.addr, 'rw');
                            })
                    )
                );
            }
        })
    ;
    
    self.element = $('<div>')
        .addClass('chat-window')
        .append(
            $('<div>')
                .addClass('chat-title')
                .text(self.contact.name)
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
                    self.contact.message(msg);
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
        self.contact.message(msg);
    };
}

