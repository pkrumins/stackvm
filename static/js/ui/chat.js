ChatWindow.prototype = new EventEmitter;
function ChatWindow (me, contact) {
    if (!(this instanceof ChatWindow)) return new ChatWindow();
    var self = this;
    
    self.contact = contact;
    
    var body = $('<div>').addClass('chat-body');
    
    self.element = $('<div>')
        .addClass('chat-window')
        .append(
            $('<div>')
                .addClass('chat-title')
                .text(contact.name)
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
    };
    
    self.say = function (msg) {
        self.addMessage(me, msg);
        contact.message(msg);
    };
}

