
var bod =
    "<form onsubmit='clickOK()'>" +
    "Please enter command line arguments:<br/>" +
    "<input type='text' id='txt' autocomplete='off'><br/>" +
    "<input type='submit' id='ok' value='OK' />" +
    "<input type='button' value='Cancel' onclick='clickCancel()' />" +
    "<div id='err' style='display:none;'><b>Error:</b> <span id='err-content'></span></div>" +
    "</form>";

var unloading = false;

$(function(){
    $("#body").replaceWith(bod);
    $("#txt").focus();
    $("#txt").change(validate);
    $("#txt").keyup(validate);
    $(window).unload(function(){
        if (!unloading) send("/cancel");
    });
});


function send(url, val) // String -> Optional String -> Maybe String
{
    var res;
    $.ajax(
        {async:false
        ,data:{arg:val}
        ,dataType:"text"
        ,url:url
        ,success:function(x){res = x}});
    return res;
}

function validate()
{
    if (unloading) return;
    var s = $("#txt").val();
    var res = send("/check", $("#txt").val());
    if (res == "") {
        $("#err").hide();
        $('#ok').removeAttr('disabled');
    } else {
        $("#err-content").text(res);
        $("#err").show();
        $('#ok').attr('disabled', 'disabled');
    }
}

function clickOK()
{
    unloading = true;
    send("/ok", $("#txt").val());
    window.close();
    return false;
}

function clickCancel()
{
    unloading = true;
    send("/cancel");
    window.close();
}
