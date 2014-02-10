"use strict";
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
 * @param {String} uri - URI to connect to.  Should look like wss://[IP of linux student environment host]:[some port]/.
 * @param {Array} key - Array of 4 words that represent the 128-bit session key.
 *
 * Implementor's note - this class must maintain consistent with
 * the code written in server.rkt.
 *
 * Invocation:
 *  ws = new SeashellWebsocket( ... )
 *
 * Code should not attempt to use the socket until after the ready Deferred
 * can be resolved.
 */
function SeashellWebsocket(uri, key) {
  var self = this;

  self.coder = new SeashellCoder(key);
  self.lastRequest = 0;
  self.requests = {};
  self.ready = $.Deferred();
  self.authenticated = false;

  // Ready to authenticate [-1]
  self.requests[-1] = {
    deferred : $.Deferred().done(self._authenticate)
  };
  // Failure [-2]
  self.requests[-2] = {
    callback : null,
    deferred : null
  };
  // Program I/O [-3]
  self.requests[-3] = { 
    callback : null,
    deferred : null
  };

  self.websocket = new WebSocket(uri);

  this.websocket.onmessage = function(message) {
    var readerT = new FileReader();
    readerT.onloadend = function() {
        var response_string = readerT.result;
        var response = JSON.parse(response_string);
        // Assume that response holds the message and response.id holds the
        // message identifier that corresponds with it.
        //
        // response.result will hold the result if the API call succeeded,
        // error message otherwise. 
        var request = self.requests[response.id];

        console.log("Received response to message with id: "+response.id+".");
        console.log(response);

        if (response.success) {
          if (request.deferred) {
            request.deferred.resolve(response.result, self);
          }
          if (request.callback) {
            request.callback(true, response.result, self);
          }
        } else {
          if (request.deferred) {
            request.deferred.reject(response.result, self);
          }
          if (request.callback) {
            request.callback(false, response.result, self);
          }
        }
        if (response.id >= 0)
           delete self.requests[response.id];
    };
    readerT.readAsText(message.data);
  };
}

/** Closes the connection.
 */
SeashellWebsocket.prototype.close = function(self) {
  this.websocket.close();
};

/** Does the client-server mutual authentication.  Internal use only. */
SeashellWebsocket.prototype._authenticate = function(ignored, self) {
  /** First send a message so we can test if the server is who he claims
   *  he is. */
  console.log("Asking for server token.");
  self._sendMessage({type : "serverAuth"}).done(
      function(response) {
        var iv = response[0];
        var coded = response[1];
        var tag = response[2];

        try {
          /** We don't care that it decrypted.
           *  We just care that it decrypted properly. */
          self.coder.decrypt(coded, iv, tag, []);
          /** OK, now we proceed to authenticate. */
          self._sendMessage({type : "clientAuth",
                             data : self.coder.encrypt(sjcl.random.randomWords(32),
                                                       [])}).done(
            function(result) {
              console.log("Authenticated!");
              self.authenticated = true;
              self.ready.resolve("Ready!");
            })
            .fail(
              function(result) {
                console.log("Server failed client token.");
                self.ready.reject("Authentication error - invalid credentials!");
              });
        } catch(error) {
          console.log("Client failed server token.");
          self.ready.reject("Authentication error - bad server credentials!");
        }
      }).fail(
        function(result) {
          console.log("Unknown error in authentication.");
          self.ready.reject("Authentication error - unknown error!");
        });
};

/** Sends a message along the connection.  Internal use only.
 *
 * @param {Object} message - JSON message to send (as JavaScript object).
 * @returns {Promise} - jQuery promise.
 */
SeashellWebsocket.prototype._sendMessage = function(message) {
  var self = this;
  // Reserve a slot for the message.
  var request_id = self.lastRequest++;
  self.requests[request_id] = message;
  message.id = request_id;
  // Stringify, write out as Array of bytes.
  var blob = new Blob([JSON.stringify(message)]);
  // Grab a deferred for the message:
  self.requests[request_id].deferred = $.Deferred();
  try {
    // Send the message:
    self.websocket.send(blob);
    return self.requests[request_id].deferred.promise();
  } catch (err) {
    return self.requests[request_id].deferred.reject(err).promise();
  }
};

/** Sends a message along the connection, ensuring that
 *  the server and client are properly authenticated. */
SeashellWebsocket.prototype.sendMessage = function(message) {
  var self = this;
  if (self.authenticated) {
    return self._sendMessage(message);
  } else {
    return $.Deferred().reject(null).promise();
  }
};

/** The following functions are wrappers around sendMessage.
 *  Consult dispatch.rkt for a full list of functions.
 *  These functions take in arguments as specified in server.rkt
 *  and return a JQuery Deferred object. */
SeashellWebsocket.prototype.runProject = function(project) {
  return this.sendMessage({
    type : "runProject",
    name : project});
};

SeashellWebsocket.prototype.compileProgram = function(project) {
  return this.sendMessage({
    type : "compileProgram",
    name : project});
};

SeashellWebsocket.prototype.getProjects = function() {
  return this.sendMessage({
    type : "getProjects"});
};

SeashellWebsocket.prototype.listProject = function(name) {
  return this.sendMessage({
    type : "listProject",
    project : name});
};

SeashellWebsocket.prototype.newProject = function(name) {
  return this.sendMessage({
    type : "newProject",
    project : name});
};

SeashellWebsocket.prototype.deleteProject = function(name) {
  return this.sendMessage({
    type : "deleteProject",
    project : name});
};

SeashellWebsocket.prototype.readFile = function(name, file_name) {
  return this.sendMessage({
    type : "readFile",
    project : name,
    file : file_name});
};

SeashellWebsocket.prototype.writeFile = function(name, file_name, file_content) {
  return this.sendMessage({
    type : "writeFile",
    project : name,
    file : file_name,
    contents : file_content});
};

SeashellWebsocket.prototype.deleteFile = function(name, file_name) {
  return this.sendMessage({
    type : "deleteFile",
    project : name,
    file : file_name});
};
