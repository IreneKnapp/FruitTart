function init() {
    $('#loginlink').bind("click", showLoginBox);
    $('#loginlink').removeAttr("href");
}

function showLoginBox() {
    $('#loginbox').show();
    var foundIt = false;
    $('#loginbox input').each(function () {
	if(foundIt) return;
	var it = $(this);
	if(it.val() == "") {
	    it.focus();
	    foundIt = true;
	}
    });
    var loginLink = $('#loginlink');
    loginLink.unbind("click", showLoginBox);
    loginLink.bind("click", hideLoginBox);
    loginLink.addClass("active");
}

function hideLoginBox() {
    $('#loginbox').hide();
    var loginLink = $('#loginlink');
    loginLink.unbind("click", hideLoginBox);
    loginLink.bind("click", showLoginBox);
    loginLink.removeClass("active");
}

$(document).ready(init);
