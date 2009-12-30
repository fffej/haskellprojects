// JavaScript module pattern
// http://yuiblog.com/blog/2007/06/12/module-pattern/

function supportsWebSockets() {
    return ("WebSocket" in window);         
}

function createConnection(open,data,close) {
    if (supportsWebSockets()) {
        var ws = new WebSocket("ws://localhost:9876/");
        
        ws.onopen = open;
        ws.onmessage = data;
        ws.onclose = close;

        return ws;
    }

    return null;
}