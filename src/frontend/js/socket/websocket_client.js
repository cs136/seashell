"use strict";
/**
 * Seashell's communications backend.
 * Copyright (C) 2013-2014 The Seashell Maintainers.
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
  can be resolved.
 */
function SeashellWebsocket(uri, key) {
  var self = this;

  self.coder = new SeashellCoder(key);
  self.lastRequest = 0;
  self.requests = {};
  self.ready = $.Deferred();
  self.authenticated = false;
  self.server_nonce = null;

  // Ready to authenticate [-1]
  self.requests[-1] = {
    deferred : $.Deferred().done(self._authenticate)
  };
  // Failure [-2]
  self.requests[-2] = {
    callback : function(s, msg) {
      displayErrorMessage(msg);
    },
    deferred : null
  };
  // Program I/O [-3]
  self.requests[-3] = { 
    callback : null,
    deferred : null
  };

  self.websocket = new WebSocket(uri);

  self.websocket.onerror = function() {
    self.requests[-2].callback(false, "An error has occurred with the websocket.");
  };

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

/** Does the client  authentication.  Internal use only. */
SeashellWebsocket.prototype._authenticate = function(server_challenge, self) {
  /** Generate a nonce. */
  var client_nonce = sjcl.random.randomWords(32);
  for (var i = 0; i < client_nonce.length; i++) {
    client_nonce[i] = client_nonce[i] & 0xFF;
  }

  /** OK, now we proceed to authenticate. */
  var raw_response = [].concat(client_nonce, server_challenge);
  var iv_coded_tag = self.coder.encrypt(raw_response, []);
  var response = [iv_coded_tag[0], iv_coded_tag[1], iv_coded_tag[2], client_nonce]; 

  self._sendMessage({type : "clientAuth",
                     response : response}).done(
    function(result) {
      console.log("Authenticated!");
      self.authenticated = true;
      self.ready.resolve("Ready!");
    })
    .fail(
      function(result) {
        self.ready.reject("Authentication error - invalid credentials!");
      });
};

/** Sends a message along the connection.  Internal use only.
 *
 * @param {Object} message - JSON message to send (as JavaScript object).
 * @param {Deferred|null} - Deferred - deferred chain to hook (optional).
 *                          If non existent, new deferred is created.
 * @returns {Promise} - jQuery promise.
 */
SeashellWebsocket.prototype._sendMessage = function(message, deferred) {
  var self = this;
  // Reserve a slot for the message.
  var request_id = self.lastRequest++;
  self.requests[request_id] = message;
  message.id = request_id;
  // Stringify, write out as Array of bytes.
  var blob = new Blob([JSON.stringify(message)]);
  // Grab a deferred for the message:
  self.requests[request_id].deferred = deferred || $.Deferred();
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
SeashellWebsocket.prototype.sendMessage = function(message, deferred) {
  var self = this;
  deferred = deferred || $.Deferred();

  if (self.authenticated) {
    return self._sendMessage(message, deferred);
  } else {
    return deferred.reject(null).promise();
  }
};

/** The following functions are wrappers around sendMessage.
 *  Consult dispatch.rkt for a full list of functions.
 *  These functions take in arguments as specified in server.rkt
 *  and return a JQuery Deferred object. */
SeashellWebsocket.prototype.ping = function(deferred) {
  return this.sendMessage({
    type : "ping"},
    deferred);
};

SeashellWebsocket.prototype.runProject = function(project, file, test, deferred) {
  return this.sendMessage({
    type : "runProject",
    project : project,
    file : file,
    test : test},
    deferred);
};

SeashellWebsocket.prototype.programKill = function(pid, deferred) {
  return this.sendMessage({
    type : "programKill",
    pid : pid},
    deferred);
};

SeashellWebsocket.prototype.sendEOF = function(pid, deferred) {
  return this.sendMessage({
    type : "sendEOF",
    pid : pid},
    deferred);
};

SeashellWebsocket.prototype.compileProject = function(project, file, deferred) {
  return this.sendMessage({
    type : "compileProject",
    project : project,
    file : file},
    deferred);
};

SeashellWebsocket.prototype.saveProject = function(project, message, deferred) {
  return this.sendMessage({
    type : "saveProject",
    project: project,
    message: message},
    deferred);
};

SeashellWebsocket.prototype.getProjects = function(deferred) {
  return this.sendMessage({
    type : "getProjects"},
    deferred);
};

SeashellWebsocket.prototype.listProject = function(name, deferred) {
  return this.sendMessage({
    type : "listProject",
    project : name},
    deferred);
};

SeashellWebsocket.prototype.newProject = function(name, deferred) {
  return this.sendMessage({
    type : "newProject",
    project : name},
    deferred);
};

SeashellWebsocket.prototype.deleteProject = function(name, deferred) {
  return this.sendMessage({
    type : "deleteProject",
    project : name},
    deferred);
};

SeashellWebsocket.prototype.lockProject = function(name, deferred) {
  return this.sendMessage({
    type : "lockProject",
    project : name},
    deferred);
};

SeashellWebsocket.prototype.forceLockProject = function(name, deferred) {
  return this.sendMessage({
    type : "forceLockProject",
    project : name},
    deferred);
};

SeashellWebsocket.prototype.unlockProject = function(name, deferred) {
  return this.sendMessage({
    type : "unlockProject",
    project : name},
    deferred);
};

SeashellWebsocket.prototype.readFile = function(name, file_name, deferred) {
  return this.sendMessage({
    type : "readFile",
    project : name,
    file : file_name},
    deferred);
};

SeashellWebsocket.prototype.newFile = function(name, file_name, deferred) {
  return this.sendMessage({
    type : "newFile",
    project : name,
    file : file_name},
    deferred);
};

SeashellWebsocket.prototype.newDirectory = function(name, dir_name, deferred) {
  return this.sendMessage({
    type : "newDirectory",
    project : name,
    dir : dir_name },
    deferred);
};

SeashellWebsocket.prototype.writeFile = function(name, file_name, file_content, deferred) {
  return this.sendMessage({
    type : "writeFile",
    project : name,
    file : file_name,
    contents : file_content},
    deferred);
};

SeashellWebsocket.prototype.deleteFile = function(name, file_name, deferred) {
  return this.sendMessage({
    type : "deleteFile",
    project : name,
    file : file_name},
    deferred);
};

SeashellWebsocket.prototype.deleteDirectory = function(name, dir_name, deferred) {
  return this.sendMessage({
    type : "deleteDirectory",
    project : name,
    dir : dir_name },
    deferred);
}

SeashellWebsocket.prototype.programInput = function(pid, contents, deferred) {
  return this.sendMessage({
    type : "programInput",
    pid : pid,
    contents : contents,
    deferred : deferred});
}

SeashellWebsocket.prototype.getExportToken = function(project, deferred) {
  return this.sendMessage({
    type : "getExportToken",
    project : project},
    deferred);
};

SeashellWebsocket.prototype.getUploadFileToken = function(project, file, deferred) {
  return this.sendMessage({
    type : "getUploadFileToken",
    project : project,
    file: file},
    deferred);
};

SeashellWebsocket.prototype.renameFile = function(project, oldName, newName, deferred) {
  return this.sendMessage({
    type: "renameFile",
    project: project,
    oldName: oldName,
    newName: newName},
  deferred);
};

SeashellWebsocket.prototype.saveSettings = function(settings, deferred) {
  return this.sendMessage({
    type : "saveSettings",
    settings : settings},
    deferred);
};

SeashellWebsocket.prototype.getSettings = function(deferred) {
  return this.sendMessage({
    type : "getSettings"},
    deferred);
};

SeashellWebsocket.prototype.marmosetSubmit = function(project, assn, deferred) {
  return this.sendMessage({
    type : "marmosetSubmit",
    project: project,
    assn: assn},
    deferred);
};
