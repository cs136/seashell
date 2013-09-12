#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include "seashell-config.h"

int main () {
  char * argv[] = { "racket", "-S", INSTALL_PREFIX "/share/collects",
                    "-l", "seashell/login", NULL };

  execvp("racket", argv);
  return 1;
}
