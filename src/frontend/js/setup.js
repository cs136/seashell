/**
 * Seashell.
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
var socket = null;
var creds = null;

/**
 * Sets up the development environment.
 */
function seashellInit(rest) {
  SeashellCoder.addEntropy();
  creds = read_login_credentials();
  if (creds) {
    socket = new SeashellWebsocket("wss://" + creds.host + ":" + creds.port, creds.key);
    socket.ready.done(function() {
      console.log("Seashell socket set up properly.");
      setupUI();
      console.log("User interface set up properly.");
      /** Install refresh handler. */
      window.onbeforeunload = function () {
        return "Are you sure you want to leave Seashell?  Unsaved data may be lost.";
      };
      /** Run the rest of the code. */
      if (rest)
        rest();
    });
    socket.ready.fail(function() {
      displayErrorMessage("Seashell socket could not be set up.");
    });
  } else {
    window.location.replace("/seashell/");
  }
}

/**
 * Sets up the user interface.
 */
function setupUI() {
  /** Clear out everything that can be hidden. */
  $(".hide-on-null-project").addClass("hide");
  $(".show-on-null-project").removeClass("hide");
  $(".hide-on-null-file").addClass("hide");
  $(".show-on-null-file").removeClass("hide");
  $(".userid").text(creds.user);
  $("#master-container").removeClass("hide"); // show the UI
  /** OK, set everything up. */
  setupEditor();
  setupConsole();
  setupDialogs();
  setupMenu();
  setupTooltips();
  setupFileMenu();
}

/** Accessor functions. */
function getSocket() {
  return socket;
}

function getCreds() {
  return creds;
}
