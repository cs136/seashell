angular.module('seashell-local-files', [])
  /**
   * Local file storage service, using localforage.js
   * Interface is pretty much the same as websocket_client.js
   */
  .service('localfiles', ['$q', 
      function($q) {
        "use strict";
        var self = this;

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
            
          // checksum is false when we're doing an offline write
          if (checksum === false) {
            $q.when(localforage.getItem(name + file_name)).then(
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
          return $q.when(localforage.setItem(name + file_name, to_write));
        };


        self.readFile = function(name, file_name) {
          return $q.when(localforage.getItem(name + file_name)).then(
            function (contents) {
              console.log("[localfiles] Reading", contents); 
              return contents; 
            });
        };


        self.deleteFile = function(name, file_name) {
          return $q.when(localforage.removeItem(name + file_name));
        };


  }]);
