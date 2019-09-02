#include <stdio.h>
#include <pwd.h>
#include <unistd.h>

int main(int argc, char **argv)
{
        // This program expects one argument, the name of a project in the
        // current term's Marmoset (the first column "Project" in the Marmoset
        // home page for the course).
        if(argc != 3) {
                fprintf(stderr, "Usage: %s <test-type> <project-name>\n", argv[0]);
                fprintf(stderr, "<test-type> is either public or secret\n");
                fprintf(stderr, "<project-name> is the name of a project from Marmoset web page\n");
                return 1;
        }

        // Get the real userid. When this program has setuid (chmod u+s)
        // set, the real userid will be the person executing it, not cs136.
        struct passwd *realpwd = getpwuid(getuid());
        if(!realpwd) { return 1; }
        // debugging
        //printf("Real username: %s\n", realpwd->pw_name);

        // Run the racket script to get test results from Marmoset database.
        char *empty_env[] = {};
        execle("/usr/bin/racket", "/usr/bin/racket", "/u/cs145/marmoset-scripts/get-test-results/get-test-results.rkt", realpwd->pw_name, argv[1], argv[2], NULL, empty_env);
        return 0;
}
