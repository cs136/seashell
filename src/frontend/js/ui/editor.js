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
var editor = null;

/** Sets up the editor. */
function setupEditor() {
  CodeMirror.registerHelper("lint", "clike", SeashellProject.linter);

  /** Values here are reasonable defaults, and will be overriden
   *  on the next call to loadConfig(); */
  editor = CodeMirror($("#editor")[0],{
    lineNumbers: true,
    tabSize: 4,
    indentUnit: 4,
    mode: "text/x-csrc",
    gutters: ["CodeMirror-lint-markers"],
    lineWrapping: true,
    matchBrackets: true,
    lint: true});
  editor.setOption("extraKeys",
      {"Ctrl-I": editorIndent,
       "Ctrl-J": editorGoto});

  editor.save = function () { SeashellProject.currentProject.save(); };
  refreshSettings();
}

function handleDocumentChange(file) {
  if (file.unsaved !== false) {
    window.clearTimeout(file.unsaved);
  }

  file.unsaved = window.setTimeout(function () {
    file.save();
   }, 1000);
}

/** Editor indent function. */
function editorIndent() {
  /** TODO: Write this, and the clang-format integration code. */
}

/** Editor goto function. */
function editorGoto() {
  /** TODO: Write this, and the dialog code that'll go with it. */
}

/** Editor undo function. */
function editorUndo() {
  editor.undo();
}

/** Editor redo function. */
function editorRedo() {
  editor.redo();
}

/** Editor force lint. */
function editorLint() {
  CodeMirror.signal(editor, "change", editor);
}

/** Editor set document.
 *  Sets the current document, and refreshes the
 *  CodeMirror instance to make sure nothing funny has happened.
 **/
function editorDocument(document) {
  editor.swapDoc(document);
  editor.refresh();
  editorLint();
}
