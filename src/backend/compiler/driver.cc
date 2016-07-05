/**
 * Seashell's LLVM and Clang interface.
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
#include "compiler.h"
#include <stdio.h>
#include "seashell-config.h"

/** This is an example usage of Seashell's clang interface,
 *  mainly for debugging memory leaks (and whatnot),
 *  and for making sure the compiler builds correctly.
 */
int main( int argc, char* argv[] ) {
  printf("Seashell %s (%s-%s) (API version %d) - clang build canary.\r\n", SEASHELL_VERSION, SEASHELL_BRANCH, SEASHELL_COMMIT_HASH, SEASHELL_API_VERSION);
  printf("Installation prefix: %s.\n", INSTALL_PREFIX);
  printf("Build directory: %s.\n", BUILD_DIR);
  printf("Debug build: %d.\n", SEASHELL_DEBUG);
  printf("Is installed: %d.\n", IS_INSTALLED());
  if (argc > 1) {
    struct seashell_preprocessor *pp = seashell_preprocessor_make();
    seashell_preprocessor_set_main_file(pp, argv[1]);
    seashell_preprocessor_run(pp);
    seashell_preprocessor_free(pp);

    struct seashell_compiler* compiler = seashell_compiler_make();
    seashell_compiler_add_file(compiler, argv[1]);
    seashell_compiler_run(compiler);
    seashell_compiler_free(compiler);
  }
}
