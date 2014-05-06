
function setupFileMenu() {
  function docSwap(data) {
    $(".hide-on-null-file").removeClass("hide");
    $(".show-on-null-file").addClass("hide");
    editorDocument(data.node.original.file.document);
 }

  $("#file-tree").jstree({})
    .on("select_node.jstree", function(e, data) {
      var prom = SeashellProject.currentProject.openFile(data.node.original.file);
      if(prom) prom.done(function() { docSwap(data); });
      else docSwap(data);
    });
}

/* proj is a SeashellProject */
function updateFileMenu(proj) {
  if(proj) {
    console.log(proj.JSTreeData());
    $.jstree.create("#file-tree", {
      'core' : {
        'data' : proj.JSTreeData()
      },
      'types' : {
        'file' : {
          'icon' : 'glyphicon glyphicon-file'
        }
      }
    });
  }
  else {
    $.jstree.create("#file-tree");
  }
}

