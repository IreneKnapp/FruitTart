function showLoginBox() {
    showElement(getObjectById("loginbox"));
    var loginLink = getObjectById("loginlink");
    loginLink.setAttribute("onclick", "hideLoginBox(); return false;");
}

function hideLoginBox() {
    hideElement(getObjectById("loginbox"));
    var loginLink = getObjectById("loginlink");
    loginLink.setAttribute("onclick", "showLoginBox(); return false;");
}

function getObjectById(objectId) {
    if(document.getElementById && document.getElementById(objectId)) {
	return document.getElementById(objectId);
    } else if (document.all && document.all(objectId)) {
	return document.all(objectId);
    } else {
	alert("Internal error, try a different browser?");
	return false;
    }
}

function hideElement(node) {
    var myStyle = node.style;
    if(myStyle) {
	if(myStyle.setAttribute) {
	    myStyle.setAttribute("display", "none");
	} else {
	    node.setAttribute("style", "display: none;");
	}
    } else {
	alert("Internal error, try a different browser?");
    }
}

function showElement(node) {
    var myStyle = node.style;
    if(myStyle) {
	if(myStyle.setAttribute) {
	    myStyle.setAttribute("display", "block");
	} else {
	    node.removeAttribute("style");
	}
    } else {
	alert("Internal error, try a different browser?");
    }
}

function addClass(element, theClass) {
  if(element.hasAttribute('class')) {
    if(!hasClass(element, theClass))
      element.setAttribute('class',
                           theClass + ' ' + element.getAttribute('class'));
  } else
    element.setAttribute('class', theClass);
}

function removeClass(element, theClass) {
  if(hasClass(element, theClass)) {
    var text = element.getAttribute('class');
    var regexp = new RegExp(' \\b' + theClass + '\\b|\\b'
                            + theClass + '\\b |\\b'
                            + theClass + '\\b', 'g');
    text = text.replace(regexp, '');
    element.setAttribute('class', text);
  }
}

function hasClass(element, theClass) {
  if(element.nodeType != Node.ELEMENT_NODE)
    return false;
  if(!element.hasAttribute('class'))
    return false;

  var regexp = new RegExp('\\b' + theClass + '\\b');
  return(regexp.test(element.getAttribute('class')));
}
