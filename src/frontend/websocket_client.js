/**
 * Seashell's communications backend.
 * Copyright (C) 2013 The Seashell Maintainers.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. 
 */
var wsUri = "TODO";

/** Seashell's Websocket Communication Class.
 * @constructor
 * @param {Array} key - Array of 4 words that represent the 128-bit AES session key.
 * 
 * Implementor's note - this class must maintain consistent with
 * the code written in server.rkt.
 *
 * TODO: Exception handling on bad keys.  Probably want to redirect
 * to the login page _again_ with a reason.
 */
function SeashellWebsocket(key) = {
  this.coder = new SeashellCoder(key);
  this.websocket = new WebSocket(wsUri);
  this.lastRequest = 0;
  this.requests = {};

  this.websocket.onmessage = function(message) {
    // We receive all messages in binary,
    // then we decrypt and extract out the nice
    // JSON.
    reader = new FileReader();
    reader.onloadend = function() {
      var result = reader.result;
 
      // Framing format (all binary bytes):
      // [IV - 12 bytes][GCM tag - 16 bytes][1 byte - Auth Len][Auth Plain][Encrypted Frame]
      // Keep this consistent with the server.
      var iv = new Uint8Array(result.slice(0,12));
      var tag = new Uint8Array(result.slice(12, 28));
      var authlen = new Uint8Array(result.slice(28, 29))[0];
      var auth = new Uint8Array(result.slice(29, 29+authlen));
      var encrypted = new Uint8Array(result.slice(29+authlen));

      // Decode plain, and verify.
      var plain = this.coder.decrypt(encrypted, iv, tag, auth);
      // Plain is an Array of bytes. Convert it into an Blob
      // and then use the FileReader class to convert that Blob
      // into a UTF-8 string.
      var blob = new Blob([new Uint8Array(plain)]);
      var reader = new FileReader();
      reader.onloadend = function() { 
        var response_string = reader.result;
        var response = JSON.parse(response_string);
        // Assume that response holds the message and response.id holds the 
        // message identifier that corresponds with it.
        var request = requests[response.id];
        request.callback(response);
        delete requests[response.id];
      }
      reader.readAsText(blob);
    }
    reader.readAsArrayBuffer(message.data); 
  };
}

/** Closes the connection.
 */
SeashellWebsocket.prototype.close = function() {
  this.websocket.close();
};

/** Sends a message along the connection, encrypting as
 * necessary.
 *
 * @param message - JSON message to send (as JavaScript object).
 */
SeashellWebsocket.prototype.sendMessage = function(message) {
  // Reserve a slot for the message.
  var request_id = this.lastRequests++;
  this.requests[request_id] = message;
  message.id = request_id;
  // Stringify, write out as Array of bytes, send.
  var blob = new Blob([JSON.stringify(message)]);
  var reader = new FileReader();
  reader.onloadend = function() {
    var frame = new Uint8Array(reader.result); 
    var plain = [];
    var result = this.coder.encrypt(frame, plain);
    var iv = result[0];
    var coded = result[1];
    var tag = result[2];

    if (plain.length > 255) {
      throw "sendMessage: Too many authenticated plaintext bytes!";
    }

    var send = iv + tag + [plain.length] + plain + coded;
    this.websocket.send(new Uint8Array(send));
  }
};
