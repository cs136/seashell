var ws = {};
var wsUri = "TODO";
var websocket;

ws.init = function() {
	websocket = new WebSocket(wsUri);
	websocket.onmessage = function(evt) {
			response = JSON.parse(evt.data);
			if (response.callback)
				response.callback(response.ret);
			websocket.close();
	};
	// TODO onopen, onerror, onclose
};

ws.getDirListing = function(base_dir, k) {
	var msg = {};
	msg.type = "getDirListing";
	msg.base_dir = base_dir;
	msg.callback = k;
	websocket.send(JSON.stringify(msg));
};

ws.loadFile = function(k, file_name) {
	var msg = {};
	msg.type = "loadFile";
	msg.file_name = file_name;
	msg.callback = k;
	websocket.send(JSON.stringify(msg));
};

ws.saveFile = function(k, file_name, file_content) {
	var msg = {};
	msg.type = "saveFile";
	msg.file_name = file_name;
	msg.file_content = file_content;
	msg.callback = k;
	websocket.send(JSON.stringify(msg));
};

ws.compileProgram = function(k, file_name) {
	var msg = {};
	msg.type = "compileProgram";
	msg.file_name = file_name;
	msg.callback = k;
	websocket.send(JSON.stringify(msg));
};

ws.runProgram = function(k, base_dir) {
	var msg = {};
	msg.type = "runProgram";
	msg.file_name = base_dir;
	msg.callback = k;
	websocket.send(JSON.stringify(msg));
};
