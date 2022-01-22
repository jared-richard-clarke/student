function createElement(name) {
	return document.createElement(name);
}

function append(target, node) {
	target.appendChild(node);
}

function createText(data) {
	return document.createTextNode(data);
}

function addListener(node, event, handler) {
	node.addEventListener(event, handler, false);
}

function insert(target, node, anchor) {
	target.insertBefore(node, anchor);
}

function setData(text, data) {
	text.data = '' + data;
}

function detachNode(node) {
	node.parentNode.removeChild(node);
}

function removeListener(node, event, handler) {
	node.removeEventListener(event, handler, false);
}
