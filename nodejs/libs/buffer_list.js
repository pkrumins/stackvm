// Treat a linked list of buffers as a single buffer.

function BufferList() {
    var head = { next : null, buffer : null };
    var last = { next : null, buffer : null };
    
    // Push a new buffer to the end of the linked list.
    this.push = function (buf) {
        if (head.buffer == null) {
            head.buffer = buf;
            last = head;
        }
        else {
            last.next = { next : null, buffer : buf };
            last = last.next;
        }
        
        return this;
    };
    
    // Take n bytes from head to the end of the concatenated buffers.
    // Returns a string.
    // If there are less than n bytes in all the buffers, returns the entire
    // concatenated buffer string.
    this.take = function (n) {
        var b = head;
        var acc = '';
        while (b && b.buffer && n > 0) {
            acc += b.buffer.toString(
                'binary',0,
                Math.min(n,b.buffer.length)
            );
            n -= b.buffer.length;
            b = b.next;
        }
        return acc;
    };
    
    this.drop = function (n) {
    };
};

exports.BufferList = BufferList;
