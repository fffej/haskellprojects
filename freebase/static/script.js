function listAlbums (band) {
    jQuery.ajax({
        success: function(msg) {
            $('#output').text('');
            if (msg['error'] !== undefined) {
                $('#output').text(msg['error']);
            } else {
                $(msg['name']).each(function(idx,val) {
                  $('#output').append('<li>' + val + '</li>');
                });
            }
        },
        url: '/albums/' + band
    });
}