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
#define _POSIX_C_SOURCE 199309L
#define _XOPEN_SOURCE 500
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <signal.h>
#include "seashell-config.h"

/** 
 * Dummy signal handler that does nothing.
 */
void dummy_signal_action(int signal, siginfo_t* info, void* context) {
  (void) signal;
  (void) info;
  (void) context;
}
/**
 * int main()
 *
 * Invokes the backend, waits until it has successfully detached from the I/O ports,
 * and then quits. 
 */
int main() {
  /** Block SIGUSR1, SIGCHLD until we can handle them. */
  sigset_t waitset;
  sigemptyset(&waitset);
  sigaddset(&waitset, SIGUSR1);
  sigaddset(&waitset, SIGCHLD);
  sigprocmask(SIG_BLOCK, &waitset, NULL);
  
  /** Install signal handlers for those signals. */
  struct sigaction actions;
  sigemptyset(&actions.sa_mask);
  actions.sa_flags = SA_SIGINFO;
  actions.sa_sigaction = dummy_signal_action;
  sigaction(SIGUSR1, &actions, NULL);
  sigaction(SIGCHLD, &actions, NULL);
  
  /** Fork the child */
  pid_t childpid = fork();
  if(childpid < 0) {
    perror("Could not fork() child:");
    return 1;
  }

  if(childpid == 0) {
    /* Detach session, setup file descriptors, spawn Racket process. */
    if(setsid() < 0) {
      perror("Could not detach session:");
      return 1;
    }

    char * argv[] = {SEASHELL_MAIN, "-s", NULL};
    execv(SEASHELL_MAIN, argv);

    perror("Could not execv() the Seashell backend:");
    return 1;
  } else {
    /* Setup file descriptors, pass key and port, exit. */
    while (true) {
      siginfo_t info;
      int result = sigwaitinfo(&waitset, &info);

      if (result < 0) {
        if (errno == EINTR) {
          continue;
        } else {
          perror("sigwaitinfo failed with:");
          return 1;
        }
      } else {
        /** Wait for the process to quit (successfully) or detach. */
        if (info.si_signo == SIGCHLD) {
          int status = 0;
          result = waitpid(childpid, &status, WCONTINUED | WUNTRACED);

          if (result < 0) {
            perror("wait failed with:");
            return 1;
          } else if (WIFEXITED(status)) {
            return WEXITSTATUS(status);
          }
        } else if (info.si_signo == SIGUSR1) {
          return 0;
        }
      }
    }
  }
}
