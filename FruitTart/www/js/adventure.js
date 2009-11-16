function init() {
    $('div#noscript').show();
    $('textarea.autosizing').bind("keyup", resizeTextarea);
    $('textarea.autosizing').each(function () { fixTextareaHeight(this); });
}


function resizeTextarea(event) {
    fixTextareaHeight(this);
}


function fixTextareaHeight(textarea) {
    $(textarea).attr("rows", 1);
    var minHeight = textarea.clientHeight;
    $(textarea).attr("rows", 2);
    $(textarea).removeAttr("rows");
    var rowHeight = textarea.clientHeight - minHeight;
    var extraHeight = minHeight - rowHeight;
    $(textarea).css("height", 0);
    var scrollHeight = textarea.scrollHeight;
    $(textarea).css("height", "auto");
    $(textarea).attr("rows", Math.floor((scrollHeight - extraHeight) / rowHeight));
}


$(document).ready(init);
