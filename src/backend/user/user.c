#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "seashell-config.h"

int main() {
  const char * ldlp = getenv("LD_LIBRARY_PATH");
  size_t sz = (ldlp ? strlen(ldlp) : 0) + strlen(INSTALL_PREFIX) + 128;
  char * bfr = malloc(sizeof(char) * sz);
  int res = snprintf(bfr, sz, "%s/lib:%s", INSTALL_PREFIX, ldlp);
  free(bfr);

  if(res >= sz || res < 0) {
    return 1;
  }

  res = setenv("LD_LIBRARY_PATH", ldlp, 1);
  if(res < 0) {
    return 1;
  }

  char * argv[] = { "racket", "-S", INSTALL_PREFIX "/share/collects",
                    "-l", "seashell/backend", NULL };

  execvp("racket", argv);
  return 1;
}

