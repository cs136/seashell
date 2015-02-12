/**
 * Seashell.
 * Copyright (C) 2013-2015 The Seashell Maintainers.
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
var seashell_console = null;

function setupConsole() {
  seashell_console = CodeMirror($("#console")[0],
      {readOnly: true,
       mode: 'text/x-markdown',
       lineWrapping: true,
       theme: 'default'});

  seashell_console.debug = false;

  $("#send-eof").on("click", sendEOF);
  $("#input-line").keydown(function(e) { if(e.which == 13) consoleInput(); });
  $("#console-clear-button").click(consoleClear);
}

function consoleDebug() {
  return seashell_console && seashell_console.debug;
}

function consoleRefresh() {
  seashell_console.refresh();
}

function consoleClear() {
  seashell_console.setValue('');
}

function consoleScrollToBottom() {
  seashell_console.scrollIntoView({line: seashell_console.lineCount() - 1,
                                   ch: 0});
}

function consoleWrite(message) {
  seashell_console.setValue(seashell_console.getValue() + message);
  consoleScrollToBottom();
}

function consoleWriteln(message) {
  consoleWrite((message || '') + '\n');
}

function consoleInput() {
  var input = $('#input-line').val() + '\n';
  $('#input-line').val('');
  SeashellProject.currentProject.input(input);
}
