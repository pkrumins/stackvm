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
                var proc = $(ui.draggable).data('proc');
                body.append($('<div>')
                    .addClass('chat-share')
                    .append(
                        $('<span>').text(proc.name),
                        $('<span>').text('Share: '),
                        $('<a>')
                            .text('[r]')
                            .click(function () {
                                $(this).parent()
                                    .empty()
                                    .text('Sharing ' + proc.name + ' (r)')
                                ;
                                var rule = {};
                                rule[contact.name] = { input : false, view : true };
                                contact.share('process', proc.addr, rule);
                            })
                        ,
                        $('<a>')
                            .text('[rw]')
                            .click(function () {
                                $(this).parent()
                                    .empty()
                                    .text('Sharing ' + proc.name + ' (rw)')
                                ;
                                var rule = {};
                                rule[contact.name] = { input : true, view : true };
                                contact.share('process', proc.addr, rule);
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
    
    self.addResource = function (contact, type, res) {
        var elem = {
            process : function () {
                return $('<p>').append(
                    contact.name + ' shares ',
                    $('<a>')
                        .text(res.filename)
                        .click(function () {
                            self.emit('attach', res);
                        })
                    ,
                    ' [' + res.mode +  ']'
                );
            },
            disk : function () {
                return $('<p>').text('Sharing disks not yet implemented');
            }
        }[type]();
        body.append(elem.addClass('chat-resource'));
    };
}

