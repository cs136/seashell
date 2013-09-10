var ws = {};
var wsUri = "TODO";
var websocket;

function SeashellWebsocket(key) = {
  this.coder = new SeashellCoder(key);
  this.websocket = new WebSocket(wsUri);

  this.websocket.onmessage = function(blob) {
    // We receive all messages in binary,
    // then we decrypt and extract out the nice
    // JSON.
    reader = new FileReader(blob);
    reader.onloadend = function() {
      var result = reader.result;
 
      // Framing format (all binary bytes):
      // [IV - 12 bytes][GCM tag - 16 bytes][1 byte - Auth Len][Auth Plain][Encrypted Frame]
      var iv = new Uint8Array(result.slice(0,12));
      var tag = new Uint8Array(result.slice(12, 28));
      var authlen = new Uint8Array(result.slice(28, 29))[0];
      var auth = new Uint8Array(result.slice(29, 29+authlen));
      var encrypted = new Uint8Array(result.slice(29+authlen));

      // Decode plain, and verify.
      var plain = coder.decrypt(encrypted, iv, tag, auth);

      // TODO: something with plain.
    } 
  };
}

// TODO - package functions up nicely.  Or not.
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

ws.revertFile = function(k, file_name) {
	var msg = {};
	msg.type = "revertFile";
	msg.file_name = file_name;
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
