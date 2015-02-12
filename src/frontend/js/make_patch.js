/*
 * Binding to make a patch of two text inputs.
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

make_patch = function (text1, text2) {
  var a1 = text1.split("\n"), a2 = text2.split("\n");
  a1.push(""); a2.push("");     // avoid nasty edge cases

  // We set up a table to help compute the Longest Common Subsequence
  // (lcs).

  // TODO: This table my become too large, we can optimize by only
  //       generating the next table row that we need inside the loop
  //       and then throw away the previous.
  var table = new Array(a1.length);
  for (var i = 0; i < table.length; i++) { table[i] = new Array(a2.length); }
  for (var i = 0; i < a1.length; i++) { table[i][0] = []; }
  for (var i = 0; i < a2.length; i++) { table[0][i] = []; }

  // Bottom up dynamic programing finds the lcs in O(a1.length *
  // a2.length) time.
  for (var i = 1; i < a1.length; i++) {
    for (var j = 1; j < a2.length; j++) {
      if (a1[i] == a2[j]) {
        table[i][j] = table[i-1][j-1].concat([a1[i]]);
      } else if (table[i-1][j].length > table[i][j-1].length) {
        table[i][j] = table[i-1][j];
      } else {
        table[i][j] = table[i][j-1];
      }
    }
  }

  // With the longest common subsequence computed, we iterate over a1,
  // a2, and lcs in lockstep (appending to diff when needed).
  var lcs = table[a1.length - 1][a2.length - 1];
  var diff = [], i = 0, j = 0, k = 0;
  while (i < a1.length || j < a2.length) {
    if (a1[i] == lcs[k]) {
      diff.push([0, lcs[k]]);
      i++; j++;
    } else {
      while (a1[i] != lcs[k+1]) {
        diff.push([-1, a1[i]]);
        i++;
      }
      while (a2[j] != lcs[k+1]) {
        diff.push([1, a2[j]]);
        j++;
      }
    }
    k++;
  }

  // We added an empty line at the start to avoid nasty edge cases,
  // now we remove that line.
  diff.length = diff.length - 1;

  // We encode the diff as an array in which a positive number
  // indicates to copy that many lines from the source to the
  // destination, a negative number indicates to skip that many lines
  // from the source, and a string should be printed as the next line
  // of the output.
  var d = 0, encoded = [];
  while (d < diff.length) {
    if (diff[d][0] == 0) {
      encoded.push(0);
      while (d < diff.length && diff[d][0] == 0) {
        encoded[encoded.length - 1] += 1;
        d++;
      }
    } else if (diff[d][0] == -1) {
      encoded.push(0);
      while (d < diff.length && diff[d][0] == -1) {
        encoded[encoded.length - 1] -= 1;
        d++;
      }
    } else if (diff[d][0] == 1) {
      encoded.push(diff[d][1]);
      d++;
    }
  }

  return encoded;
}
