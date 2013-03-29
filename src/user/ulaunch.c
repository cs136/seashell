/*--------------------------------------------------------------------
Seashell
Copyright (C) 2012-2013 Jennifer Wong, Marc Burns

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Authors: Jennifer Wong, Marc Burns
---------------------------------------------------------------------*/
#include <sys/unistd.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <time.h>
#include <errno.h>
//#include "config.h"

/* Current environment. */
extern char * const environ[];

int main(int argc, char * argv[])
{
  /* Start child process. */
  {
    int childpid = fork();
    if(!childpid) {
      {
        /* Detach and start new process group. */
        int nulfd = open("/dev/null", O_RDWR);
        close(0);
        close(1);
        close(2);
        dup2(nulfd, 0);
        dup2(nulfd, 1);
        dup2(nulfd, 2);
        close(nulfd);
        setsid();
      }

      /* Install signal handlers. */
      {
        struct sigaction act;
        act.sa_handler = SIG_IGN;
        act.sa_flags = 0;
        sigemptyset(&act.sa_mask);
        sigaction(SIGINT, &act, NULL);
        sigaction(SIGTERM, &act, NULL);
        sigaction(SIGHUP, &act, NULL);
        sigaction(SIGQUIT, &act, NULL);
        sigaction(SIGALRM, &act, NULL);
        sigaction(SIGPIPE, &act, NULL);
        sigaction(SIGCHLD, &act, NULL);
      }

      /* Invoke payload. */
      char * const args[] = { "bash", NULL };
      execve("/u/m4burns/public_html/test/payload",args,environ);
    } else {
      /* Print header and child PID, then exit. */
      printf("Content-type: text/html\n\nHello!\nChild: %d\n", childpid);
      fflush(stdout);
    }
  }

  exit(0);
}

