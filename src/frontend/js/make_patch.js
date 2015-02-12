/*
 * Binding to make a patch of two text inputs.
 * Copyright (C) 2015 Kieran Colford
 *                    The Seashell Maintainers
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

/**
 * make_patch(text1, text2)
 * Computes the patch P that sends text1 -> text2
 */
function make_patch (text1, text2) {
  var a1 = text1.split("\n"), a2 = text2.split("\n");
  var table = new Array(a1.length);
  var i = 0, j = 0;
  for (i = 0; i < table.length; i++) { table[i] = new Array(a2.length); }
  for (i = 0; i < a1.length; i++) { table[i][0] = 0; }
  for (i = 0; i < a2.length; i++) { table[0][i] = 0; }

  // Two cases:
  //  We can extend the LCS of T1[..i-1], T2[..j-1] to T1[..i], T2[..j]
  //  Otherwise, copy from LCS T1[..i-1], T2[..j], T1[..i], T2[..j-1]
  for (i = 0; i < a1.length; i++) {
    for (j = 0; j < a2.length; j++) {
      if (a1[i] === a2[j]) {
        table[i][j] = (i && j && table[i-1][j-1] + 1) || 0
      } else if (j === 0 || (i > 0 && table[i-1][j] > table[i][j-1])) {
        table[i][j] = (i && table[i-1][j]) || 0;
      } else {
        table[i][j] = (j && table[i][j-1]) || 0;
      }
    }
  }

  var result = [];
  for(i = a1.length - 1, j = a2.length - 1; i >= 0 || j >= 0;) {
    // Backtrack the LCS
    //
    // Case 1: LCS extended
    if (i >= 0 && j >= 0 && a1[i] === a2[j]) {
      i --; j --;
      if (result.length > 0 && typeof result[0] === 'number' && result[0] > 0) 
        result[0]++;
      else
        result.unshift(1);
    }
    // Case 2: LCS not extended, added line to the LCS from T2
    else if (j >=0 && (i < 0 || ((j && table[i][j - 1]) || 0) >= ((i && table[i-1][j]) || 0))) {
      result.unshift(a2[j]);
      j --;
    }
    // Case 3: LCS not extended, removed line from T1 
    else {
      i -- ;
      if (result.length > 0 && typeof result[0] === 'number' && result[0] < 0) 
        result[0] --;
      else
        result.unshift(-1);
    }
  }

  return result;
}

console.log(make_patch("Hi\nThis is some text\nCommon","This is some text\nCommon\nFoo\n"));
