/**
 * Seashell's LLVM and Clang interface.
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
#include "compiler.h"

/** This is an example usage of Seashell's clang interface,
 *  mainly for debugging memory leaks (and whatnot).
 */
int main( int argc, char* argv[] ) {
  if (argc > 1) {
    struct seashell_compiler* compiler = seashell_compiler_make();
    seashell_compiler_add_file(compiler, argv[0]);
    seashell_compiler_run(compiler);
    seashell_compiler_free(compiler);
  }
}
