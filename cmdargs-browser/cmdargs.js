
var bod =
    "<form onsubmit='ok()'>" +
    "Please enter command line arguments:<br/>" +
    "<input type='text' id='txt' autocomplete='off'><br/>" +
    "<input type='submit' value='OK' />" +
    "<input type='button' value='Cancel' onclick='cancel()' />" +
    "</form>";

var unloading = false;

$(function(){
    $("#body").replaceWith(bod);
    $("#txt").focus();
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


function ok()
{
    send("/ok", $("#txt").val());
    unloading = true;
    window.close();
}

function cancel()
{
    send("/cancel");
    unloading = true;
    window.close();
}
