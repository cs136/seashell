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
var seashell_console = null;

/** Sets up the console. */
function setupConsole() {
  var welcomeMessage = '**Welcome to Seashell! Messages and program output will appear here.**\n';
  seashell_console = CodeMirror($('#console')[0],
      {value: welcomeMessage,
       readOnly: true,
       lineWrapping: true,
       theme: 'blackboard'});
}
