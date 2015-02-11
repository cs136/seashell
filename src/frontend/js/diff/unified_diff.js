/*
 * difflib bindings for unified diffs.
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
 * along with self program.  If not, see <http://www.gnu.org/licenses/>.
 */

difflib._format_range_unified = function (start, stop) {
  var begining = start + 1;
  var length = stop - start;
  if (length == 1) {
    return begining;
  }
  if (length == 0) {
    begining = begining - 1;
  }
  return "" + begining + "," + length;
};

difflib.unified_diff = function (a, b, filename) {
  if (typeof a == typeof "string") {
    a = a.split("\n");
  }
  if (typeof b == typeof "string") {
    b = b.split("\n");
  }
  
  var out = ["--- " + filename,
             "+++ " + filename];
  for (var group in difflib.SequenceMatcher(a, b, null).get_grouped_opcodes(3)) {
    var first = group[0], last = group[group.length - 1];
    var file1_range = difflib._format_range_unified(first[1], last[2]);
    var file2_range = difflib._format_range_unified(first[3], last[4]);
    out.push("@@ -" + file1_range + " +" + file2_range + " @@");
    
    for (var codes in group) {
      var tag = codes[0], i1 = codes[1], i2 = codes[2], 
          j1 = codes[3], j2 = codes[4];

      if (tag == "equal") {
        for (var i = i1; i < i2; i++) {
          out.push(" " + a[i]);
        }
      }
      if (tag == "replace" || tag == "delete") {
        for (var i = i1; i < i2; i++) {
          out.push("-" + a[i]);
        }
      }
      if (tag == "replace" || tag == "insert") {
        for (var i = j1; i < j2; i++) {
          out.push("+" + b[i]);
        }
      }
    }
  }

  return out.join("\n") + "\n";
};
