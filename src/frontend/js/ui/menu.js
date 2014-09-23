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

function handleSettingsDialog() {
    refreshSettings();
}

function handleSaveProject() {
  return SeashellProject.currentProject.save();
}

function handleCompileProject() {
  return SeashellProject.currentProject.compile();
}

function handleRunProject() {
  consoleClear();
  $("#input-line").focus();
  return SeashellProject.run();
}

function handleRunTests() {
  consoleClear();
  $("#input-line").focus();
  return SeashellProject.runTests();
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

  $("#seashell-logo").on("click", handleCloseProject);
  $("#settings-dialog").on("click", handleSettingsDialog);

  $("#toolbar-run").on("click", handleRunProject);
  $("#toolbar-run-tests").on("click", handleRunTests);
  $("#toolbar-kill").on("click", handleProgramKill);
  $('#toolbar-delete-file').on("click", function() {
    displayConfirmationMessage('delete file',
                               'are you sure you want to delete the current\
                               file?',
                  function() {
                    var file = SeashellProject.currentProject.currentFile;
                    if (!file.is_dir)
                      SeashellProject.currentProject.deleteFile(file)
                      .done(updateFileMenu);
                  });
  });

  $('#toolbar-add-file').on('click', showNewFileDialog);
  $('#toolbar-rename-file').on('click', showRenameMoveFileDialog);

  $('#toolbar-submit-question').on('click', function() {
    var marm;
    if(marm = SeashellProject.currentProject.currentMarmosetProject()) {
      displayConfirmationMessage("Marmoset Submit", "Would you like to submit your code to the Marmoset project "+marm+"?", function() {
        handleMarmosetSubmit(marm);
      });
    }
    else {
      $('#marmoset-submit-dialog').modal('show');
    }
  });
  $("#toolbar-submit-results").click(showMarmosetDetailsDialog);

  $('#common-files').css('visibility', 'hidden');
  $('#tests-files').hide();
}

function updateFileMenu()
{
  openQuestion(SeashellProject.currentProject.currentQuestion)
    .done(focusCurrentFile);
}

function updateQuestionsMenu(proj)
{
  socket.listProject(proj.name).done(function(files) {
    var questions = _.chain(files)
          .filter(function(x) {
            var name = x[0];
            return x[1] && 'common' != name && -1 == name.indexOf('/');
          })
            .map(function(x) { return x[0]; })
              .sortBy(_.identity)
                .value();
    $('#questions-row').empty();
    var links =
          _.map(questions, function(name) {
            var link = $('<a href="#" class="question-link">' + name + '</a>');
            link.click(function(x) {
              SeashellProject.currentProject.closeFile(false);
              openQuestion(name);
              var link = this;
              _.forEach($('.question-link-active'),
                        function(x) { x.className = 'question-link'; });
              link.className = 'question-link-active';
            });
            return link;
          });
    _.forEach(links, function(link) {
      _.forEach([link, ' '], function(x) { $('#questions-row').append(x); });
    });
    if (links.length)
      links[0].click();
    if (links.length < 2)
      $('#questions-row').hide();
    else
      $('#questions-row').show();
  });
}



function openQuestion(qname)
{
  consoleClear();
  editorClear();

  $(".hide-on-null-file").addClass("hide");
  $(".show-on-null-file").removeClass("hide");

  var p = SeashellProject.currentProject;
  var result = $.Deferred();
  if(!qname) {
    result.reject();
    return result;
  }
  socket.listProject(p.name).done(function(files) {
    function attach_dir_listing_to_node(dir, parent) {
      function basename(z) { return z.split('.')[0]; }
      function extension(z) { return z.split('.')[1]; }
      function dirname(z) {
        // Two cases [for backwards compatibility with old versions of Seashell]
        var result = /\/[^\/]+$/.exec(z);

        if (result) {
          return z.substring(0, z.length - result[0].length);
        } else {
          return ""; // Fallback case [when you hit files in the root
                     // directory]. It's possible that students will
                     // open projects created with old versions of
                     // Seashell, and at least this won't break the
                     // interface badly.
        }
      }
      var qfiles =
        _.chain(files)
          .filter(function(x) { return !x[1] && dirname(x[0]) == dir; })
            .map(function(x) { return /[^\/]+$/.exec(x[0])[0]; })
              .value();
      function source_buddy_for(x) {
        return { 'c' : 'h', 'in' : 'expect' }[x];
      }
      function has_source_buddy(x)
      {
        var source_buddies = ['c', 'h', 'in', 'expect'];
        return source_buddies.indexOf(extension(x)) >= 0 &&
          _.find(qfiles,
                 function(y) { return x != y &&
                               -1 != source_buddies.indexOf(extension(y)) &&
                               basename(x) == basename(y); });
      }
      function make_file_link(x, caption)
      {
        caption = caption || x;
        var path = dir + '/' + x;
        var link = $('<a href="#" class="file-link"\
                     style="text-decoration: none">' + caption + '</a>');
        link.append($('<input>', { type: 'hidden',
                                   value: path,
                                   class: 'files-list-crumb' }));
        link.click(function() {
          var link = this;
          p.openFilePath(path).done(function(x) {
            _.forEach($('.file-link-active'),
                      function(x) { x.className = 'file-link'; });
            link.className = 'file-link-active';
            updateDynamicUISizes();
          });
        });
        return link;
      }

      parent.empty();
      return _.chain(qfiles)
        .map(function(x) {
          var elt = $('<span>', { style: 'margin-right: 1em;' });
          if (!has_source_buddy(x))
          {
            var link = make_file_link(x);
            elt.append(link);
            parent.append(elt);
            parent.append(' ');
            return link;
          }
          if (-1 == ['c', 'in'].indexOf(extension(x)))
            return null;
          var link =
            make_file_link(x, basename(x) + '&nbsp;' + extension(x));
          elt.append(link);
          elt.append('<span style="color: #aaa; font-size: 12px">,</span>');

          var buddy = source_buddy_for(extension(x));
          elt.append(make_file_link(basename(x) + '.' + buddy, buddy));
          parent.append(elt);
          parent.append(' ');
          return link;
        })
          .filter(_.identity).value();
    }

    $('#question-files-list-title').text(qname + '/');
    $('#folder-option-current-question').text(qname);
    $(".hide-on-null-question").removeClass("hide");
    $(".show-on-null-question").addClass("hide");

    attach_dir_listing_to_node(qname, $('#question-files-row'));
    attach_dir_listing_to_node('common', $('#common-files-row'));
    attach_dir_listing_to_node(qname + '/tests', $('#test-files-row'));

    function dir_has_files(dir) {
      return _.find(files,
                    function (x) { return !x[0].indexOf(dir) && !x[1]; });
    }
    $('#common-files')
      .css('visibility', dir_has_files('common') ? 'visible' : 'hidden');
    if (dir_has_files(sprintf('%s/tests', qname)))
      $('#tests-files').show();
    else
      $('#tests-files').hide();

    p.currentQuestion = qname;
    focusCurrentFile();

    result.resolve();
  });

  return result;
}

function focusCurrentFile()
{
  var crumbs = $('.files-list-crumb');
  if (!crumbs.length)
    return;
  var p = SeashellProject.currentProject;
  var file = p.currentFile;
  var crumb =
        file
        && _.find(crumbs, function(x) { return file.fullname() == x.value; })
        || $(sprintf('.files-list-crumb[value^="%s"]', p.currentQuestion))[0]
        || crumbs[0];
  $(crumb).parent().click();
}

function handleCloseProject() {
  SeashellProject.currentProject.close().done(function() {
    $(".hide-on-null-project").addClass("hide");
    $(".show-on-null-project").removeClass("hide");
    $(".hide-on-null-file").addClass("hide");
    $(".show-on-null-file").removeClass("hide");
    $(".hide-on-null-question").addClass("hide");
    $(".show-on-null-question").removeClass("hide");
  }).fail(function() {
    displayErrorMessage("Could not successfully close assignment.");
  });
}

function updateProjectsDropdown(proj)
{
  var dropdown = $('#projects-dropdown');
  dropdown.empty();
  function add_menuitem(caption, handler) {
    var li = $('<li role="presentation"></li>');
    var link = $('<a role="menuitem" tabindex="-1" href="#"></a>');
    link.text(caption);
    link.on('click', handler);
    li.append(link);
    dropdown.append(li);
    return li;
  }
  function add_divider() {
    dropdown.append('<li role="presentation" class="divider"></li>');
  }
  add_menuitem('download assignment…', handleDownloadProject);
  add_menuitem('new question…',
               function() { $('#new-folder-dialog').modal('show'); });

  add_divider();
  add_menuitem('close assignment', handleCloseProject);

  if (! /^[aA][0-9]+/.test(proj.name))
    add_menuitem('delete assignment', function() {
      displayConfirmationMessage
      ("Delete Assignment", "Are you sure you want to delete this assignment?",
       handleDeleteProject);
    });
}
