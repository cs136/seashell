/** glue between UI and websockets **/
function revertFile(file_name) {
	return ws_revertFile(file_name);
}

function getFile(file_name) {
    return ws_getFile(file_name);
}

function getDirListing(directory) {
    return ws_getDirListing(directory);
}

function runProgram(file_name) {
	return ws_runProgram(file_name);
}

// compileProgram() does not touch the UI at all and should only be
// called by compileHandler().
function compileProgram(file_name) {
	/*
	var dfd = $.Deferred();

	// TODO: this should be directory
	ws_compileProgram(None).then(
			dfd.resolve,
			dfd.reject);
			*/
	return ws_compileProgram(file_name);
}

function saveFile(file_name, file_content, history) {
	// TODO history is not currently persistent
	return ws_saveFile(file_name, file_content);
}

function commitFile(file_name, file_content, history) {
	// TODO history is not currently persistent
	return ws_commitFile(file_name, file_content);
}

function ws_revertFile(file_name) {
	var dfd = $.Deferred();
	dfd.resolve({file_name: file_name, success: true, messages: new Array("sadface"), file_content: "you reverted the file " + file_name});
	return dfd;
}

function ws_getFile(file_name) {
	var dfd = $.Deferred();
	dfd.resolve({file_name: file_name, success: true, messages: new Array(), file_content: "you got the file " + file_name});
	return dfd;
}

function ws_getDirListing(directory) {
	var dfd = $.Deferred();
	dfd.resolve({success: true, messages: new Array(), file_list: [["f", "asdf"], ["d", "durr"]]});
	return dfd;
}

function ws_runProgram(file_name) {
	var dfd = $.Deferred();
	dfd.resolve({file_name: file_name, success: true, messages: new Array()});
	return dfd;
}

function ws_compileProgram(file_name) {
	var dfd = $.Deferred();
	dfd.resolve({file_name: file_name, success: true, messages: new Array()});
	return dfd;
}

function ws_saveFile(file_name, file_content) {
	var dfd = $.Deferred();
	dfd.resolve({file_name: file_name, success: true, messages:new Array()});
	return dfd;
}

function ws_commitFile(file_name, file_content) {
	var dfd = $.Deferred();
	dfd.resolve({file_name: file_name, success: true, messages:new Array()});
	return dfd;
}

