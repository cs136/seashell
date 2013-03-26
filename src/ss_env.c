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
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <pwd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Drop to nobody. */
#define DROP_USER "nobody"
#define DROP_GROUP "nobody"

/* Chroot jail path. */
#define CHROOT_PATH "/opt/seashell/chroot"

/* Compile utility. */
#define COMPILER_PATH "/opt/seashell/compile.sh"

/* Run utility, relative to the chroot jail. */
#define RUNNER_PATH "/bin/run.sh"

/* File copy buffer size. */
#define BUFSZ 512

#define perrorf(...) do { fflush(stdout); fprintf(stderr, ##__VA_ARGS__); } while(0)
#define perrorstrf(fmt, ...)\
  do {\
    char bfr[256];\
    strerror_r(errno, bfr, 256);\
    bfr[255] = 0;\
    fflush(stdout);\
    fprintf(stderr, fmt, bfr, ##__VA_ARGS__);\
  } while(0)
#define die(...) do { perrorf(__VA_ARGS__); exit(1); } while(0)

/* Drop privileges. */
int drop_priv(int drop_uid, int drop_gid) {
	/* Empty the supplementary groups list. */
	if(setgroups(0, NULL) != 0) {
		perrorstrf("Error emptying the supplementary groups list: %s.\n");
		return -1;
	}
	if(setgid(drop_gid) != 0) {
		perrorstrf("Error on setgid(drop_gid): %s.\n");
		return -1;
	}
	if(setuid(drop_uid) != 0) {
		perrorstrf("Error on setuid(drop_uid): %s.\n");
		return -1;
	}
	if(chdir("/") != 0) {
		perrorstrf("Error on chdir('/'): %s.\n");
		return -1;
	}
	return 0;
}

/* Arguments to this program are expected to be
	the executable name followed by all source files. */

char ** environ;

int main(int argc, char * argv[])
{
	pid_t pid;
	int status, rv, i, drop_uid, drop_gid;

	/* Sanity checks. */
	if(geteuid() != 0) {
		die("This program must be run as root.\n");
	}
	if(argc < 3) {
		die("This program must be run with at least two arguments.\n");
	}

	/*** THIS CODE RUNS AS EUID 0 ***/
	/* Create a new session and process group. */
	pid = fork();
	if(pid) {
		status = 0;
		while((rv = waitpid(pid, &status, 0)) == -1 && errno == EAGAIN);
		if(rv < 0) {
			perrorstrf("Error waiting on initial child: %s\n");
			exit(1);
		} else {
			exit(WEXITSTATUS(status));
		}
		/* The parent is now dead. */
	} else {
		setsid();
	}

  /* Get UID and GID under which to run processes. */
  {
    char buf[256];
    struct passwd pws, *pw;
    struct group grs, *gr;
    if(0 != getpwnam_r(DROP_USER, &pws, buf, 256, &pw)) {
      perrorstrf("Error on getpwnam_r: %s.\n");
      exit(1);
    }
    if(!pw) {
      die("getpwnam(%s) failed.\n", DROP_USER);
    }
    drop_uid = pw->pw_uid;

    if(0 != getgrnam_r(DROP_GROUP, &grs, buf, 256, &gr)) {
      perrorstrf("Error on getgrnam_r: %s.\n");
      exit(1);
    }
    if(!gr) {
      die("getgrnam(%s) failed.\n", DROP_GROUP);
    }
    drop_gid = gr->gr_gid;

    if((drop_uid == 0) || (drop_gid == 0)) {
      die("Neither DROP_USER nor DROP_GROUP may be root.\n");
    }
  }


	/* Start compiler process. */
	pid = fork();
	if(pid) {
		status = 0;
		while((rv = waitpid(pid, &status, 0)) == -1 && errno == EAGAIN);
		if(rv < 0) {
			perrorstrf("Error waiting on compile child: %s\n");
			exit(1);
		} else {
			if(WEXITSTATUS(status) != 0) {
				exit(WEXITSTATUS(status));
			}
		}
		/* Parent continues execution after the following block. */
	} else {
		if(drop_priv(drop_uid, drop_gid) != 0) {
			die("Could not drop privileges.\n");
		}
		/* No longer running as root. Execute the compile. */
		{
			char ** new_argv = alloca(sizeof(char*) * (argc + 1));
			new_argv[0] = COMPILER_PATH;
			for(i=1; i<argc; i++) {
				new_argv[i] = argv[i];
			}
			new_argv[argc] = NULL;
			if(execve(COMPILER_PATH, new_argv, environ) != 0) {
				perrorstrf("Error in execve (compile): %s\n");
				exit(1);
			}
		}
	}

	/* The binary should now exist. Sanity check. */
	{
		int fd, ofd, n, m, mypid;
		struct stat st;
		char rpath[PATH_MAX], bindest[PATH_MAX], buf[BUFSZ];

		/* 1. Is the binary in /tmp? */
		if(realpath(argv[1], rpath) == NULL) {
			perrorstrf("Error on realpath: %s.\n");
			exit(1);
		}
		if(strstr(rpath, "/tmp/") != rpath) {
			die("Binary is not in /tmp.\n");
		}

		/* 2. Is the binary executable and
		 * owned by drop_uid:drop_gid? */
		if((fd = open(rpath, O_RDONLY|O_NOCTTY)) < 0) {
			perrorstrf("Error opening compiler output: %s.\n");
			exit(1);
		}
		if(fstat(fd, &st) != 0) {
			perrorstrf("Could not fstat() compiler output: %s.\n");
			exit(1);
		}
		if(!(st.st_mode & S_IXUSR)) {
			die("Compiler output is not executable.\n");
		}
		if(st.st_uid != drop_uid) {
			die("Compiler output owner does not match drop_uid.\n");
		}
		if(st.st_gid != drop_gid) {
			die("Compiler output group does not match drop_gid.\n");
		}

		/* Output is sane. Copy it to the chroot jail. */
		mypid = getpid();
		if(snprintf(bindest, PATH_MAX, "%s/tmp/seashell%d", CHROOT_PATH, mypid) >= PATH_MAX) {
			die("Binary path in chroot jail is too long.\n");
		}
		if((ofd = open(bindest, O_CREAT|O_EXCL|O_WRONLY, 0755)) < 0) {
			perrorstrf("Could not open output binary in chroot jail: %s.\n");
			exit(1);
		}
		if(fchown(ofd, drop_uid, drop_gid) != 0) {
			perrorstrf("Could not set output binary owner and group: %s.\n");
			exit(1);
		}

		while(((n = read(fd, buf, BUFSZ)) > 0) || (n == -1 && errno == EAGAIN)) {
			while(n > 0) {
				if((m = write(ofd, buf, n)) < 0) {
					if(errno != EAGAIN) {
						perrorstrf("Could not write to output binary: %s.\n");
						exit(1);
					} else {
						continue;
					}
				}
				n -= m;
			}
		}
    if(n == -1) {
      perrorstrf("Error reading binary: %s.\n");
      exit(1);
    }
		close(fd);
		close(ofd);

		/* Binary is in the chroot jail. Enter jail, drop privileges, and run. */
		if(chdir(CHROOT_PATH) != 0) {
			perrorstrf("Could not chdir to jail: %s.\n");
      exit(1);
		}
		if(chroot(CHROOT_PATH) != 0) {
			perrorstrf("Could not chroot to jail: %s.\n");
      exit(1);
		}

		/* Start runner process. */
		pid = fork();
		if(pid) {
			status = 0;
			while((rv = waitpid(pid, &status, 0)) == -1 && errno == EAGAIN);
			if(rv < 0) {
				perrorstrf("Error waiting on compile child: %s\n");
				exit(1);
			} else {
				exit(WEXITSTATUS(status));
			}
			/* The parent is now dead. */
		} else {
			if(drop_priv(drop_uid, drop_gid) != 0) {
				die("Could not drop privileges.\n");
			}
			/* No longer running as root. Execute the run. */
			snprintf(bindest, PATH_MAX, "/tmp/seashell%d", mypid);
			{
				char ** new_argv = alloca(sizeof(char*) * 3);
				new_argv[0] = RUNNER_PATH;
				new_argv[1] = bindest;
				new_argv[2] = NULL;
				if(execve(RUNNER_PATH, new_argv, environ) != 0) {
					perrorstrf("Error in execve (run): %s\n");
					exit(1);
				}
			}
		}
	}
	/* Silence the warning. */
	return 0;
}
