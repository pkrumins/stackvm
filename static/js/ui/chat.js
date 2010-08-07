function ChatWindow () {
    if (!(this instanceof ChatWindow)) return new ChatWindow();
    var self = this;
    
    self.element = $('<div>')
        .addClass('chat-window')
    ;
}

