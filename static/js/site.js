$(document).ready(function () {
    DNode.connect(function (remote) {
        remote.session(function (err, account) {
            if (!err) Hash(account.disks).forEach(registerDisk);
        });
    });
});

function registerDisk (disk, filename) {
    var diskElem = $('<div>')
        .addClass('proc')
        .text(filename)
        .appendTo($('#disks'))
    ;
    
    function registerProc (proc) {
        var thumb = $('<div>');
        function makeImg () {
            var uri = '/disks/' + filename + '/' + proc.address + '/thumbnail';
            return $('<img>').attr('src', uri).width(200).height(150);
        }
        
        var procElem = $('<div>')
            .addClass('proc')
            .append(makeImg)
            .appendTo(diskElem)
        ;
        proc.subscribe(function (sub) {
            sub.on('exit', function () {
                thumb.remove();
            });
            
            sub.on('thumb', function () {
                thumb.find('img').remove();
                thumb.append(makeImg());
            });
        });
    }
    
    Hash(disk.processes).forEach(registerProc);
    
    disk.subscribe(function (sub) {
        sub.on('spawn', registerProc);
    });
}
