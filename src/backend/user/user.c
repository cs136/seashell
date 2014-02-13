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
#include <string.h>
#include <errno.h>
#include "seashell-config.h"

// Child (racket) process died?
volatile int dead;
#define CHECK_DEAD do { if(dead) { fprintf(stderr, "Died unexpectedly.\n"); exit(1); } } while(0)

/**
 * sig_handler(int num)
 *
 * Signal handler.  Handles unexpected SIGINT/SIGTERM/SIGQUIT/SIGPIPE
 * signals from the child.
 */
void sig_handler(int num) {
  switch(num) {
    case SIGINT:
    case SIGTERM:
    case SIGQUIT:
    case SIGPIPE:
    case SIGCHLD:
      dead = 1;
      break;
    default:;
  }
}

/**
 * int main()
 *
 * Main function.  Sets up two-way communications with Racket to get authentication information,
 * and then detaches.
 */
int main() {
  signal(SIGINT, sig_handler);
  signal(SIGTERM, sig_handler);
  signal(SIGQUIT, sig_handler);
  signal(SIGPIPE, sig_handler);
  signal(SIGCHLD, sig_handler);

  // Communication pipes - P is from us to Racket, Q
  // is from Racket to us (stdout).  We leave
  // alone standard error.
  int p[2], q[2];
  if(pipe(p) < 0 || pipe(q) < 0) {
    fprintf(stderr, "Could not allocate pipes.\n");
    exit(1);
  }

  pid_t childpid = fork();
  if(childpid < 0) {
    fprintf(stderr, "Could not fork().\n");
    exit(1);
  }

  if(!childpid) {
    /* Detach session, setup file descriptors, spawn Racket process. */
    if(setsid() < 0) {
      fprintf(stderr, "Could not detach session.\n");
      exit(1);
    }

    /* Close the output end of P and the input end of Q. */
    close(p[1]);
    close(q[0]);

    /** Leave stderr as-is.  Process detaches from it anyways, and getting the traceback
     *  from initial startup is probably nice.*/
    close(0);
    close(1);

    dup2(p[0], 0);
    dup2(q[1], 1);

    close(p[0]);
    close(q[1]);

    char * argv[] = {"seashell-user-launcher", NULL};
    errno = execv(SEASHELL_USER_LAUNCHER, argv);

    fprintf(stderr, "exec failed (errno=%d)\n", errno);
    exit(1);
  } else {
    /* Setup file descriptors, pass key and port, exit. */
    int res;

    close(p[0]);
    close(q[1]);

    char bfr[128];
    size_t num_read = 0, num_write = 0;

    while ((res = read(0, bfr + num_read, 16 - num_read))) {
      CHECK_DEAD;
      if(res < 0) {
        if(res != EINTR) {
          fprintf(stderr, "Error reading session key (errno=%d)\n", errno);
          exit(1);
        }
      } else {
        num_read += res;
      }
    }

    if (num_read < 16) {
      fprintf(stderr, "Input stream closed before session key was read.\n");
      exit(1);
    }

    while (num_write < num_read) {
      res = write(p[1], bfr + num_write, num_read - num_write);
      CHECK_DEAD;
      if(res < 0) {
        if(res != EINTR) {
          fprintf(stderr, "Error writing session key (errno=%d)\n", errno);
          exit(1);
        }
      } else {
        num_write += res;
      }
    }

    num_read = num_write = 0;
    while ((res = read(q[0], bfr + num_read, 128 - num_read))) {
      CHECK_DEAD;
      if(res < 0) {
        if(res != EINTR) {
          fprintf(stderr, "Error reading port number (errno=%d)\n", errno);
          exit(1);
        }
      } else {
        num_read += res;
        if(bfr[num_read-1] == '\n') {
          break;
        }
      }
    }

    if ((num_read == 0) || (bfr[num_read-1] != '\n')) {
      fprintf(stderr, "Input stream closed before port number was read.\n");
      exit(1);
    }

    while (num_write < num_read) {
      res = write(1, bfr + num_write, num_read - num_write);
      CHECK_DEAD;
      if(res < 0) {
        if(res != EINTR) {
          fprintf(stderr, "Error writing port number (errno=%d)\n", errno);
          exit(1);
        }
      } else {
        num_write += res;
      }
    }

    close(p[1]);
    close(q[0]);
    exit(0);
  }
}
