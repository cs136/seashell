/**
 * Seashell's authentication and communications backend.
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
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/prctl.h>
#include <string.h>
#include <errno.h>
#include "seashell-config.h"

/** This file launches the main Seashell launcher process with
 *  prctl(PR_SET_DUMPABLE, 0) and with effective GID set to allow
 *  read-access to the shared SSL keys.
 *
 *  Seashell should _not_ in any case execute any student code
 *  in its address space.
 *
 *  This file should be chmod 2755 and chown root:seashell-data
 */
int main(void) {
  if (prctl(PR_SET_DUMPABLE, 0, 0, 0, 0) != 0) {
    fprintf(stderr, "Could not disable ptrace!\n");
    return 1;
  }

  char * argv[] = { SEASHELL_RACKET, "-S", INSTALL_PREFIX "/share/collects",
                    "-l", "racket/base", "-l", "seashell/backend", "-e",
                    "(backend-main)", NULL };
  // SECURITY NOTE:
  //  SEASHELL_RACKET MUST BE AN ABSOLUTE PATH OR ELSE
  //  AN ARBITRARY FILE CAN BE EXECUTED HERE.  THIS WILL LEAK
  //  SSL KEYS.
  return execv(SEASHELL_RACKET, argv);
}
