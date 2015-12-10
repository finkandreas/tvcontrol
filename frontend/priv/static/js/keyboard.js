$(function() {
    $(".symbol").click(function(ev) {
        var keycode = Number($(this).attr("keycode"));
        if (keycode) GUI.sendWebsocket("keypress", keycode);
    });
});
