/**
 * Seashell's frontend error service
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

angular.module('frontend-app')
  // Error service.
  .service('error-service', ['$rootScope', '$timeout', 
    function ($rootScope, $timeout) {
      
    var self = this;

    // global variables
    self.errors = [];
    self.logs = [];

    // how many entries to keep in self.logs
    var logSize = 200;

    // override console.log so that it also logs to self.logs
    logConsoleFn("log");
    logConsoleFn("warn");
    logConsoleFn("error");

    // makes the UI display an error message
    self.report = function (error, shorthand, type) {
      if (error) {
        console.error(error);
        self.errors.push({shorthand: shorthand, error: error, type: type});
        $timeout(function() {$rootScope.$broadcast('window-resized');}, 0);
      }
    };

    // close the error message
    self.suppress = function (index) {
      self.errors.splice(index, 1);
      $timeout(function() {$rootScope.$broadcast('window-resized');}, 0);
    };

    // returns a pretty string of self.logs
    self.dumpLogs = function() {
      return "navigator.appVersion: " + navigator.appVersion + "\n\n" +
        self.logs.map(function(entry) {
            var prefix;
            switch (entry.type) {
                case "log": prefix   = "  log > "; break;
                case "warn": prefix  = " warn ! "; break;
                case "error": prefix = "error * "; break;
            }
            return prefix + entry.text;
        }).join("\n");
    };

    // display self.dumpLogs() in a new open window
    self.dumpLogsNewWindow = function() {
        return window.open().document.write("<pre>"+self.dumpLogs()+"</pre>");
    };

    // extend default js console.log so that it also logs to self.log
    function logConsoleFn(fn) {
        var defaultFn = window.console[fn];
        window.console[fn] = function() {
            defaultFn.apply(window.console, arguments);
            self.logs.push({type: fn, text: prettyLogMsg.apply(this, arguments)});
            if (self.logs.length > logSize) {
                self.logs.shift();
            }
        };
    }

    // format a console.log message to human readable string
    // eg. prettyLogMsg(object1, object2, object3, ...);
    function prettyLogMsg() {
        return Array.prototype.slice.call(arguments).map(function(arg) {
            try {
                // in case the object is recursive
                return JSON.stringify(arg);
            } catch (e) {
                return JSON.stringify(e);
            }
        }).join(" ");
    }

    // bug report email format
    self.reportTo = "seashell@cs.uwaterloo.ca";
    self.reportSubject = encodeURIComponent("Seashell issue report");
    self.reportBody = function() {
        return encodeURIComponent("[Tell us what you were doing when this error showed up.]\n\n" + self.dumpLogs());
    };

}]);
