/**
 * Seashell's authentication and communications backend.
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
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdarg.h>
#include "seashell-config.h"

 char* make_message(const char *fmt, ...)
 {
    int n;
    int size = 100;     /* Guess we need no more than 100 bytes. */
    char *p, *np;
    va_list ap;
    if ((p = malloc(size)) == NULL)
        return NULL;
    while (1) {
        /* Try to print in the allocated space. */
        va_start(ap, fmt);
        n = vsnprintf(p, size, fmt, ap);
        va_end(ap);
        /* If that worked, return the string. */

        if (n > -1 && n < size)
            return p;
        /* Else try again with more space. */
        if (n > -1)    /* glibc 2.1 */
            size = n+1; /* precisely what is needed */
        else           /* glibc 2.0 */
            size *= 2;  /* twice the old size */
        if ((np = realloc (p, size)) == NULL) {
            free(p);
            return NULL;
        } else {
            p = np;
        }
    }
}
/**
 * seashell_signal_detach(void)
 * Signals to the backend process that they can detach (successfully)
 * now.
 */
int seashell_signal_detach(void) {
  /** Reopen stdout to /dev/null, stderr to $HOME/.seashell/seashell.log,
   *  and stdin to /dev/zero.
   *  Note that the file would have already existed.
   */ 
  int error = 0;

  char *logfd_path = make_message("%s/cs136/seashell/seashell.log", getenv("HOME"));
  if (logfd_path == NULL) {
    error = 1;
    goto end;
  }

  int logfd = open(logfd_path, O_WRONLY | O_APPEND, 0);
  if (logfd < 0) {
    error = errno;
    goto end;
  }
  int nullfd = open("/dev/null", O_RDWR, 0);
  if (nullfd < 0) {
    error = errno;
    goto end;
  }

  /** dup2 into the file descriptors. */
  if (dup2(logfd, 2) < 0) {
    error = errno;
    goto end;
  }
  if (dup2(nullfd, 1) < 0) {
    error = errno;
    goto end;
  }
  if (dup2(nullfd, 0) < 0) {
    error = errno;
    goto end;
  }

  if (kill(getppid(), SIGUSR1) < 0) {
    error = errno;
    goto end;
  }

end:
  if (logfd >= 0)
    close(logfd);
  if (nullfd >= 0)
    close(nullfd);
  free(logfd_path);
  return error;
}
