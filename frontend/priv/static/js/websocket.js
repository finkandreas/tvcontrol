$(function() {
    var websocket = new WebSocket("ws://"+window.location.host+"/ws");
    websocket.onopen = function(evt) {
      console.log("onOpen event");
    }

    websocket.onclose = function(evt) {
      console.log("onclose event");
    }

    websocket.onmessage = function(evt) {
      console.log("onMessage event", evt);
    }

    websocket.onerror = function(evt) {
      console.log("onError event", evt);
    }
});
