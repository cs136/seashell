angular.module('seashell-local-files', [])
  /**
   * Local file storage service, using localforage.js
   * Interface is pretty much the same as websocket_client.js
   */
  .service('localfiles', ['$q', '$cookies',
    function($q, $cookies) {
      "use strict";
      var self = this;

      self.user = $cookies.getObject(SEASHELL_CREDS_COOKIE).user;
      // set up localforage to have a per-user store
      // note that this doesn't actually secure anything:
      // it only prevents name conflicts
      localforage.config({
        name: 'SeashellStorage', // "database name"
        storeName: self.user     // "table name"
      }); 

      /*
       * Returns the path to where this file is stored.
       */
      self._path = function(project, file) {
        return project + file;
      };

      /*
       * Save a file to local storage.
       * @param {string} name: project name
       * @param {string} file_name: filename
       * @param {string} file_content: The contents of the file
       * @param {string | false} checksum: MD5 checksum of the contents,
       *   or false for an offline-write
       */
      self.writeFile = function(name, file_name, file_content, checksum) {
        var offline_checksum = md5(file_content);
        var online_checksum;
        var path = self._path(name, file_name);
          
        // checksum is false when we're doing an offline write
        if (checksum === false) {
          $q.when(localforage.getItem(path)).then(
              function (contents) {
                  online_checksum = contents.online_checksum;
              }
          );
        }

        var to_write = { 
          data: file_content, 
          online_checksum: checksum || online_checksum, 
          offline_checksum: offline_checksum 
        }; 
        console.log("[localfiles] Writing: ", to_write);
        return $q.when(localforage.setItem(path, to_write));
      };

      self.readFile = function(name, file_name) {
        return $q.when(localforage.getItem(self._path(name, file_name))).then(
          function (contents) {
            console.log("[localfiles] Reading", contents); 
            return contents; 
          });
      };

      self.deleteFile = function(name, file_name) {
        return $q.when(localforage.removeItem(self._path(name, file_name)));
      };

      self.getRunnerFile = function(name, question) {
        return $q.when(localforage.getItem(self._path(name, question) + "//runnerFile"))
          .then(function(contents) {
            return contents;
          });
      };

      self.setRunnerFile = function(name, question, folder, file) {
        if(folder=="common" || folder=="tests")
          return $q.reject("Runner file must be in question directory.");
        return $q.when(localforage.setItem(self._path(name, question) + "//runnerFile"), file);
      };

  }]);
