/* Seashell (C) 2012 Jenny Wong, Marc Burns.
 * Source compile/execute wrapper. This file must be compiled
 * and installed setuid root. */
#include <sys/unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Drop to nobody. */
#define DROP_UID 99
#define DROP_GID 99

/* Chroot jail path. */
#define CHROOT_PATH "/home/marc/seashell/src/chroot"

/* Compile utility. */
#define COMPILER_PATH "/home/marc/seashell/src/asdf.sh"

/* Run utility, relative to the chroot jail. */
#define RUNNER_PATH "/asdf.sh"

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
int drop_priv() {
	/* Empty the supplementary groups list. */
	if(setgroups(0, NULL) != 0) {
		perrorstrf("Error emptying the supplementary groups list: %s.\n");
		return -1;
	}
	if(setgid(DROP_GID) != 0) {
		perrorstrf("Error on setgid(DROP_GID): %s.\n");
		return -1;
	}
	if(setuid(DROP_UID) != 0) {
		perrorstrf("Error on setuid(DROP_UID): %s.\n");
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
	int status, rv, i;

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
		if(drop_priv() != 0) {
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
		 * owned by DROP_UID:DROP_GID? */
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
		if(st.st_uid != DROP_UID) {
			die("Compiler output owner does not match DROP_UID.\n");
		}
		if(st.st_gid != DROP_GID) {
			die("Compiler output group does not match DROP_GID.\n");
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
		if(fchown(ofd, DROP_UID, DROP_GID) != 0) {
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
			if(drop_priv() != 0) {
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
