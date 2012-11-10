"use strict";

var unloading = false;

$(function(){
    $("#body").replaceWith(createModes(mode));
    $(".link:first").click();
    $("#txt").focus();
    $("#txt").change(validate).keyup(validate);
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

var activeMode = "";

function space(a, b)
{
    return a === "" || b === "" ? a + b : a + " " + b;
}

function quote(s)
{
    return /^[A-Za-z0-9_=\-\"]+$/.test(s) ? s : "\"" + s + "\"";
}

function updateCmdLine()
{
    var res = activeMode;
    $(".main .part").each(function(){
        res = space(res, $(this).data("part")());
    });
    $("#txt").val(res);
    validate();
}

/////////////////////////////////////////////////////////////////////
// CONTENT CREATION

function createModes(m)
{
    $(".main input").live("input", updateCmdLine);
    return div([div({"class":"left"}, createTree(m,null))
              ,div({"class":"main"}, [])
              ,div({"class":"bottom"}, createCmdLine())
              ]);
}

function createTree(m,path)
{
    var path2 = path === null ? "" : space(path, m.names[0]);
    var xs = [];
    var lnk = div({"class":"link"},m.names[0]);
    xs.push(lnk);
    var mod = createMode(m);
    lnk.click(function(){
        $(".main").children().detach();
        $(".main").append(mod);
        $(".active").removeClass("active");
        lnk.addClass("active");
        activeMode = path2;
        updateCmdLine();
    });

    for (var i = 0; i < m.modes.unnamed.length; i++)
        xs.push(createTree(m.modes.unnamed[i], path2));
    for (var i = 0; i < m.modes.named.length; i++)
    {
        xs.push(bold(m.modes.named[i][0]));
        for (var j = 0; j < m.modes.named[i][1].length; j++)
            xs.push(createTree(m.modes.named[i][1][j], path2));
    }

    return div({"class":"group"},xs);
}

function createCmdLine()
{
    return $(
        "<form onsubmit='clickOK()'>" +
        "Command line arguments:<br/>" +
        "<input type='text' id='txt' autocomplete='off'><br/>" +
        "<input type='submit' id='ok' style='font-weight:bold;' value='OK' />" +
        "<input type='button' value='Cancel' onclick='clickCancel()' />" +
        "<div id='err' style='display:none;'><b>Error:</b> <span id='err-content'></span></div>" +
        "</form>");
}

function createMode(m)
{
    var trs = [];
    function row(a,b,c)
    {
        var res = b === undefined
                  ? tr(td({colspan:3},a))
                  : tr({"class":"top"},[td({style:"padding-top:5px;text-align:right;"},a),td(b),td({"class":"doc",style:"padding-left:15px;padding-top:5px;"},c)]);
        trs.push(res);
        return res;
    }

    row(m.help);

    // Arguments
    var reqArgs = m.args[0];
    var optArgs = m.args[1];
    if (reqArgs.length !== 0 || optArgs !== null)
        row(h3("Arguments"));

    for (var i = 0; i < reqArgs.length; i++)
    {
        var a = reqArgs[i];
        row("Argument " + i, [], textField(a.type), a.require ? "required" : "");
    }
    if (optArgs !== null)
        row("Arguments", entries(optArgs.type, quote), [], optArgs.require ? "required" : "");

    function addFlag(flag)
    {
        var name = (flag.names[0].length === 1 ? "-" : "--") + flag.names[0];
        var it = italic({"class":"part"}, "No value");
        it.data("part", function(){return name;});

        var part = function(s){
            return quote(name + (flag.info.type === "rare" && s == "" ? "" : "=") + s);
        };
        row(tt(name), entries(flag.type, part, flag.info.type === "none" ? it : undefined), flag.help);
    }

    var unnamedFlags = m.flags.unnamed;
    if (unnamedFlags.length > 0)
        row(h3("Flags"));
    for (var i = 0; i < unnamedFlags.length; i++)
        addFlag(unnamedFlags[i]);

    var namedFlags = m.flags.named;
    for (var i = 0; i < namedFlags.length; i++)
    {
        row(h3(namedFlags[i][0]));
        for (var j = 0; j < namedFlags[i][1].length; j++)
            addFlag(namedFlags[i][1][j]);
    }

    if (m.helpSuffix.length > 0)
        row(h3("Help information"));
    for (var i = 0; i < m.helpSuffix.length; i++)
        row(div({style:"white-space:pre-wrap;"},escapeHTML(m.helpSuffix[i])));

    return table(trs);
}

function entries(help, part, custom)
{
    var add = addButton();
    var bot = tr([td(add), td({width:"150px","class":"doc",style:"padding-top:3px;"},help)]);
    add.click(function(){
        var del = delButton();
        var inp = input({"class":"part"},[]);
        inp.data("part", function(){return part(inp.val());});
        var x = tr([td(del), td(custom === undefined ? inp : custom.clone(true))]);
        bot.before(x);
        if (custom === undefined)
            inp.focus();
        del.click(function(){
            x.remove();
            updateCmdLine();
        });
        updateCmdLine();
    });
    return table({"class":"entries"},bot);
}

function textField(typ)
{
    return input({value:typ},[]);
}

function addButton()
{
    return div({"class":"add"},"")
}

function delButton()
{
    return div({"class":"del"},"")
}

/////////////////////////////////////////////////////////////////////
// TAG LIBRARY

function tag(name, attrs, content)
{
    if (content == undefined)
    {
        content = attrs;
        attrs = undefined;
    }
    var res = $("<" + name + ">");

    function addAttr(xs)
    {
        if ($.isArray(xs))
        {
            for (var i = 0; i < xs.length; i++)
                addAttr(xs[i]);
        }
        else
        {
            for (var i in xs)
                res.attr(i, xs[i]);
        }
    }

    function addContent(xs)
    {
        if ($.isArray(xs))
        {
            for (var i = 0; i < xs.length; i++)
                addContent(xs[i]);
        }
        else
            res.append(xs);
    }

    if (attrs !== undefined)
        addAttr(attrs);
    if (content !== undefined)
        addContent(content);
    return res;
}

function div(attrs, content){return tag("div", attrs, content);}
function tt(attrs, content){return tag("tt", attrs, content);}
function td(attrs, content){return tag("td", attrs, content);}
function tr(attrs, content){return tag("tr", attrs, content);}
function table(attrs, content){return tag("table", attrs, content);}
function h3(attrs, content){return tag("h3", attrs, content);}
function italic(attrs, content){return tag("i", attrs, content);}
function bold(attrs, content){return tag("b", attrs, content);}
function input(attrs, content){return tag("input", attrs, content);}
function hyperlink(attrs, content){return tag("a", attrs, content);}

function escapeHTML(x)
{
    return x;
}
