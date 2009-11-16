function init() {
    $('div#noscript').show();
    $('textarea.autosizing').bind("keyup", resizeTextarea);
    $('textarea.autosizing').each(function () { fixTextareaHeight(this); });
    $('input[type=checkbox][name=is-template-expression]')
      .bind("click", toggleIsTemplateExpression);
    $('div.query-button').wrapInner("<div></div>");
    $('div.query-button.add').bind("click", addRow);
    $('div.query-button.remove').bind("click", removeRow);
    $('div.query-button.up').bind("click", moveRowUp);
    $('div.query-button.down').bind("click", moveRowDown);
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


function addRow(event) {
    var row = $(this).parent().parent();
    var newRow = $('#query-row').clone(true);
    var parent = row.parent();
    newRow.removeAttr('id');
    row.before(newRow);
    row.each(fixButtons);
    newRow.each(fixButtons);
    newRow.prev().each(fixButtons);
    fixNames(parent);
}


function removeRow(event) {
    var row = $(this).parent().parent();
    var prevRow = row.prev();
    var nextRow = row.next();
    var parent = row.parent();
    row.remove();
    prevRow.each(fixButtons);
    nextRow.each(fixButtons);
    fixNames(parent);
}


function moveRowUp(event) {
    var row = $(this).parent().parent();
    var prevRow = row.prev();
    var parent = row.parent();
    if(prevRow.length != 0) {
	prevRow.before(row);
	row.each(fixButtons);
	prevRow.each(fixButtons);
	fixNames(parent);
    }
}


function moveRowDown(event) {
    var row = $(this).parent().parent();
    var nextRow = row.next().not(':last-child');
    var parent = row.parent();
    if(nextRow.length != 0) {
	nextRow.after(row);
	row.each(fixButtons);
	nextRow.each(fixButtons);
	fixNames(parent);
    }
}


function fixButtons() {
    $(this).children('td').children('div.query-button, br').remove();
    
    var isFirstRow = ($(this).filter(':first-child').length == 1);
    var isLastRow = ($(this).next().filter(':last-child').length == 1);
    var isTailRow = ($(this).filter(':last-child').length == 1);
    var isProperRow = !isTailRow;
    var hasUpButton = isProperRow && !isFirstRow;
    var hasDownButton = isProperRow && !isLastRow;
    var hasBreak = hasUpButton && hasDownButton;
    var hasAddButton = true;
    var hasRemoveButton = isProperRow;
    
    var node;
    
    if(hasUpButton) {
	node = $('#query-button-up').clone(true);
	node.removeAttr('id');
	$(this).children('td:first-child').append(node);
    }
    
    if(hasBreak) {
	$(this).children('td:first-child').append($(document.createElement('br')));
    }
    
    if(hasDownButton) {
	node = $('#query-button-down').clone(true);
	node.removeAttr('id');
	$(this).children('td:first-child').append(node);
    }
    
    if(hasRemoveButton) {
	node = $('#query-button-remove').clone(true);
	node.removeAttr('id');
	$(this).children('td:first-child + td').prepend(node);
    }
    
    if(hasAddButton) {
	node = $('#query-button-add').clone(true);
	node.removeAttr('id');
	$(this).children('td:first-child + td').prepend(node);
    }
}


function fixNames(tbody) {
    var rows = tbody.children();
    var i = 1;
    rows.each(function() {
	var popupName = "type" + i;
	var inputName = "name" + i;
	$(this).find('select').attr('name', popupName);
	$(this).find('input').attr('name', inputName);
	i++;
    });
}


function toggleIsTemplateExpression() {
    if($('input[type=checkbox][name=is-template-expression]').val()) {
	$('div#results').hide();
	$('#body-label').text('Template Expression:');
    } else {
	$('div#results').show();
	$('#body-label').text('SQL:');
    }
}


$(document).ready(init);
