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
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/prctl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include "seashell-config.h"

/** This file provides a few auxiliary helper functions for
 *  ensuring that Seashell does not leak secure secret keys.
 */

/**
 * seashell_drop_permissions(void)
 * Disables tracing and drops effective group ID.
 * Do this after loading the SSL keys, and before anything else happens.
 */
int seashell_drop_permissions(void) {
  int error = 0;
  if ((error = prctl(PR_SET_DUMPABLE, 0, 0, 0, 0)) != 0) {
    fprintf(stderr, "Could not disable ptrace on ourself - %d!\n", error);
    return 1;
  }

  /** Note: PR_SET_DUMPABLE must be set 0 before this call
   *  goes through as if not, user process can ptrace
   *  Seashell and possibly extract private keys. */
  if ((error = setresgid(getgid(), getgid(), getgid()) != 0)) {
    fprintf(stderr, "Could not setresgid(%d, %d, %d) - %d!\n",
        getgid(), getgid(), getgid(), error);
    return 1;
  }

  return 0;
}
