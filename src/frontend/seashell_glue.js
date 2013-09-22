/** glue between UI and websockets **/
function revertFile(file_name) {
	return ws.revertFile(file_name);
}

function getFile(file_name) {
    return ws.getFile(file_name);
}

function getDirListing(directory) {
    return ws.getDirListing(directory);
}

function runProgram(file_name) {
	return ws.runProgram(file_name);
}

// compileProgram() does not touch the UI at all and should only be
// called by compileHandler().
function compileProgram(base_dir) {
	return ws.compileProgram(base_dir);
}

function saveFile(file_name, file_content, history) {
	// TODO history is not currently persistent
	return ws.saveFile(file_name, file_content);
}

function commitFile(file_name, file_content, history) {
	// TODO history is not currently persistent
	return ws.commitFile(file_name, file_content);
}

/*
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
*/
