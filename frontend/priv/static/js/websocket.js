var GUI = GUI || {};
GUI.MessagesToSend = [];

$(function() {
    var websocket = new WebSocket("ws://"+window.location.host+"/ws");
    websocket.onopen = function(evt) {
      console.log("onOpen event");
      for (var i=0; i<GUI.MessagesToSend.length; ++i) GUI.sendWebsocket(GUI.MessagesToSend[i].cmd, a[i].arg);
      GUI.MessagesToSend = [];
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

    GUI.sendWebsocket = function(cmd, arg) {
      if (websocket.readyState == websocket.OPEN) {
        websocket.send(JSON.stringify({cmd: cmd, arg: arg}));
      } else {
        console.log("Websocket is not ready yet. Data cannot be sent yet.");
        GUI.MessagesToSend.push({cmd: cmd, arg: arg});
      }
    }
});
