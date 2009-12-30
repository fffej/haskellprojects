// JavaScript module pattern
// http://yuiblog.com/blog/2007/06/12/module-pattern/

function supportsWebSockets() {
    return ("WebSocket" in window);         
}

function createConnection() {
    if (supportsWebSockets()) {
        var ws = new WebSocket("ws://localhost:9876/");
        
        ws.onopen = function() {
            alert('opened');
        };

        ws.onmessage = function(evt) {
            alert(evt.data);
        };

        ws.onclose = function() {
            alert('closed');
        };

        return ws;
    }

    return null;
}