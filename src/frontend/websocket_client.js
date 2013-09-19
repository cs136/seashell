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

/** Seashell's Websocket Communication Class.
 * @constructor
 * @param {String} uri - URI to connect to.  Should look like ws://[IP of linux student environment host]:[some port]/.  Encryption is not handled through SSL.
 * @param {Array} key - Array of 4 words that represent the 128-bit AES session key.
 *
 * Implementor's note - this class must maintain consistent with
 * the code written in server.rkt.
 *
 * TODO: Exception handling on bad keys.  Probably want to redirect
 * to the login page _again_ with a reason.
 */
function SeashellWebsocket(uri, key) {
  var self = this;

  self.coder = new SeashellCoder(key);
  self.lastRequest = 0;
  self.requests = {};
  self.ready = false;
  // Ready [-1]
  self.requests[-1] = {
    callback : function() {
      self.ready = true;
    }
  };
  // Failure [-2]
  self.requests[-2] = {
    callback : function () {
      self.failed = true;
    }
  };
  self.read_ctr = 0;
  self.write_ctr = 0;
  self.websocket = new WebSocket(uri);

  this.websocket.onmessage = function(message) {
    // We receive all messages in binary,
    // then we decrypt and extract out the nice
    // JSON.
    var readerT = new FileReader();
    console.log(readerT);
    readerT.onloadend = function() {
      console.log(readerT);
      var result = readerT.result;

      // Framing format (all binary bytes):
      // [CTR - 2 bytes]
      // [IV - 12 bytes]
      // [GCM tag - 16 bytes]
      // [1 byte - Auth Len]
      // [Auth Plain]
      // [Encrypted Frame]
      // Keep this consistent with the server.
      var ctr = new Uint8Array(result.slice(0,2));
      var auth_typed = new Uint8Array(result.slice(31, 31+authlen));
            var auth = [ctr[0], ctr[1]]
            ctr = ctr[0] << 8 | ctr[1];
            for(var auth_index = 0; auth_index < auth_typed.length; auth_index++) {
              auth.push(auth_typed[auth_index]);
            }
      var iv = new Uint8Array(result.slice(2,14));
      var tag = new Uint8Array(result.slice(14, 30));
      var authlen = new Uint8Array(result.slice(30, 31))[0];
      var encrypted = new Uint8Array(result.slice(31+authlen));

      // Check counters
      if (ctr != self.read_ctr)
        throw "Bad counter received!";

      self.read_ctr = (self.read_ctr + 1) % 65536;

      // Decode plain, and verify.
      var plain = self.coder.decrypt(encrypted, iv, tag, auth);
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

        // TODO: Error handling!
        var request = self.requests[response.id];
        request.callback(response);

        if (response.id > 0)
         delete requests[response.id];
      }
      reader.readAsText(blob);
    }
    readerT.readAsArrayBuffer(message.data);
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
  var self = this;
  // Reserve a slot for the message.
  var request_id = self.lastRequest++;
  self.requests[request_id] = message;
  message.id = request_id;
  // Stringify, write out as Array of bytes, send.
  var blob = new Blob([JSON.stringify(message)]);
  var reader = new FileReader();
  reader.onloadend = function() {
    // Keep in mind the framing format:
    // [CTR - 2 bytes]
    // [IV - 12 bytes]
    // [GCM tag - 16 bytes]
    // [1 byte - Auth Len]
    // [Auth Plain]
    // [Encrypted Frame]
    // Keep this consistent with the server.
    var ctr = self.write_ctr;
    self.write_ctr = (self.write_ctr + 1) % 65536;
        ctr = [(ctr >> 8) & 0xFF, ctr & 0xFF];
    var frame = new Uint8Array(reader.result);
    var plain = [];
    var authenticated = ctr;
    authenticated.concat(plain);
    var result = self.coder.encrypt(frame, authenticated);
    var iv = result[0];
    var coded = result[1];
    var tag = result[2];

    if (plain.length > 255) {
      throw "sendMessage: Too many authenticated plaintext bytes!";
    }

    var send = [];
    [ctr, iv, tag, [plain.length], plain, coded].forEach(
      function (arr) {
        send = send.concat(arr);
      });
    self.websocket.send(new Uint8Array(send));
  };
  reader.readAsArrayBuffer(blob);
};
