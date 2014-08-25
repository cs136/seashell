"use strict";

function setupFileMenu() {
  function docSwap(doc) {
    $(".hide-on-null-file").removeClass("hide");
    $(".show-on-null-file").addClass("hide");
    editorDocument(doc);
  }
  $.jstree.create("#file-tree", {
    'core' : {
      'data' : (SeashellProject.currentProject ?
        SeashellProject.currentProject.JSTreeData() : [])
    },
    'plugins' :
      ['contextmenu'],
    'contextmenu' : {
      'items' : function(node) {
        return {
          'Rename' : {
            'label' : 'Rename',
            'icon' : 'glyphicon glyphicon-edit',
            'action' : function(obj) {
              var file = SeashellProject.currentProject.getFileFromPath(node.original.path);
              $("#rename-file-old-input").val(file.fullname());
              $("#rename-file-new-input").val(file.name[file.name.length-1]);
              $("#rename-file-dialog").modal("show");
            }
          },
          'Delete' : {
            'label' : 'Delete',
            'icon' : 'glyphicon glyphicon-remove',
            'action' : function(obj) {
              var file = SeashellProject.currentProject.getFileFromPath(node.original.path);
              if(file.is_dir) {
                displayConfirmationMessage("Delete Folder",
                  "Are you sure you want to delete the folder \""+file.fullname()+"\"? This will delete all files and folders it contains.",
                  function() {
                    SeashellProject.currentProject.deleteFile(file)
                      .done(updateFileMenu);
                  });
              }
              else {
                SeashellProject.currentProject.deleteFile(file)
                  .done(function() {
                    updateFileMenu();
                    $(".hide-on-null-file").addClass("hide");
                    $(".show-on-null-file").removeClass("hide");
                  });
              }
            }
          }
        }
      }
    }
  });
  $("#file-tree").jstree()
    .on("select_node.jstree", function(e, data) {
      var file = SeashellProject.currentProject.getFileFromPath(data.node.original.path);
      if(!file.is_dir) {
        var prom = SeashellProject.currentProject.openFile(file);
        if(prom) prom.done(function() { docSwap(file.document); });
        else docSwap(file.document);
        if(file.unsaved) {
          $("#edit-tab-item").text("*Editor*");
        }
        else {
          $("#edit-tab-item").text("Editor");
        }
        $("#last-saved").text("Last saved "+file.lastSavedString());
      }
    })
    .on("rename_node.jstree", function(e, data) {
      var file = SeashellProject.currentProject.getFileFromPath(data.node.original.path);
      SeashellProject.currentProject.renameFile(file)
        .fail(function() {
          displayErrorMessage("File or folder "+file.fullname()+" could not be renamed.");
          updateFileMenu();
        })
        .done(function() {
          file.name[file.name.length-1] = e.new;
        });
    });
}

/* proj is a SeashellProject */
function updateFileMenu(proj) {
  setupFileMenu();
}

