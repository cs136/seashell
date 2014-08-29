"use strict";
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

function handleSaveProject() {
  return SeashellProject.currentProject.save();
}

function handleCompileProject() {
  return SeashellProject.currentProject.compile();
}

function handleRunProject() {
  return SeashellProject.run();
}

function handleRunTests() {
  SeashellProject.currentProject.compile().done(function() {
    var tests = SeashellProject.currentProject.getTestsForFile
      (SeashellProject.currentProject.currentFile);

    function run_tests() {
      if (!tests.length) {
        consoleWriteln('# done');
        setPlayStopButtonPlaying(false);
        return;
      }
      var name = tests.shift();
      consoleWrite(sprintf("# run test '%s'... ", name));
      SeashellProject.currentProject.run('../q1/tests/' + name)
        .fail(function() { console.log('oops test failed'); })
        .done(function(result) {
          consoleWrite(result.tag);
          if ('fail' == result.tag)
            consoleWrite(sprintf(' with output:\n%s', result.data.actual));
          consoleWriteln();
          run_tests();
        });
    }
    run_tests();
  });
}

function handleProgramKill() {
  SeashellProject.currentProject.kill()
    .done(function() {
      setPlayStopButtonPlaying(false);
      editor.focus();
      consoleWriteln("# stopped by user (that's you!)");
    });
}

function handleDownloadProject() {
  SeashellProject.currentProject.getDownloadToken()
    .done(function(token) {
      $("#download-iframe").remove();
      var raw = JSON.stringify(token);
      var frame = $("<iframe>").attr("src",
        sprintf("https://%s:%s/export/%s.zip?token=%s",
          creds.host,
          creds.port,
          encodeURIComponent(SeashellProject.currentProject.name),
          encodeURIComponent(raw)))
        .attr("id", "download-iframe");
      $("#download-project-body").append(frame);
      $("#download-project-dialog").modal("show");
    });
}

function setPlayStopButtonPlaying(playing) {
  var a = '#toolbar-run', b = '#toolbar-kill';
  if (!playing)
    b = [a, a = b][0];
  $(a).addClass('hidden');
  $(b).removeClass('hidden');
}

function setupMenu() {
  function withInputFromConsole(x) {
    consoleClear();
    $('#input-line').focus();
    x();
  }

  $("#toolbar-run").on("click", function() {
    withInputFromConsole(handleRunProject);
  });
  $("#toolbar-run-tests").on("click", function() {
    withInputFromConsole(handleRunTests);
  });
  $("#toolbar-kill").on("click", handleProgramKill);
  $("#menu-download").on("click", handleDownloadProject);
}
