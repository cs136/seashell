
function setupFileMenu() {
  function docSwap(doc) {
    $(".hide-on-null-file").removeClass("hide");
    $(".show-on-null-file").addClass("hide");
    editorDocument(doc);
  }
  $(":jstree").jstree("destroy");
  $.jstree.create("#file-tree", {
    'core' : {
      'data' : (SeashellProject.currentProject ?
        SeashellProject.currentProject.JSTreeData() : []),
      'check_callback': true
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
              $("#file-tree").jstree().rename_node(obj);
            }
          },
          'Delete' : {
            'label' : 'Delete',
            'icon' : 'glyphicon glyphicon-remove',
            'action' : function(obj) {
              var file = SeashellProject.currentProject.getFileFromPath(node.original.path);
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
  });
  $("#file-tree").jstree()
    .on("select_node.jstree", function(e, data) {
      var file = SeashellProject.currentProject.getFileFromPath(data.node.original.path);
      var prom = SeashellProject.currentProject.openFile(file);
      if(prom) prom.done(function() { docSwap(file.document); });
      else docSwap(file.document);
    });
}

/* proj is a SeashellProject */
function updateFileMenu(proj) {
  setupFileMenu();
}

