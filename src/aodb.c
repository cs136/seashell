#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <time.h>

#define AODB_MAGIC 0xdeadbeef
#define perrorf(...) do { fflush(stdout); fprintf(stderr, __VA_ARGS__); } while(0)
#define perrorstrf(fmt, ...)\
  do {\
    char bfr[256];\
    strerror_r(errno, bfr, 256);\
    bfr[255] = 0;\
    fflush(stdout);\
    fprintf(stderr, fmt, bfr, ##__VA_ARGS__);\
  } while(0)

struct db_context {
  int fd_read, fd_append;
};

struct __attribute__((packed)) db_record {
  int magic;
  int tv_sec, tv_nsec;
  int length;
  char * data_ptr; // Need this to deal with Racket, sadly.
  char end[];
};

int db_init(struct db_context * ctx, const char * path) {
  int rv = open(path, O_CREAT|O_APPEND|O_WRONLY, 0600);
  if(rv < 0) {
    perrorstrf("aodb: Error opening database for append: %s\n");
    ctx->fd_read = ctx->fd_append = -1;
    return -1;
  }
  ctx->fd_append = rv;

  rv = open(path, O_RDONLY);

  if(rv < 0) {
    perrorstrf("aodb: Error opening database for read: %s\n");
    close(ctx->fd_append);
    ctx->fd_read = ctx->fd_append = -1;
    return -1;
  }
  ctx->fd_read = rv;

  return 0;
}

void db_close(struct db_context * ctx) {
  close(ctx->fd_append);
  close(ctx->fd_read);
  ctx->fd_append = ctx->fd_read = -1;
  sync();
}

int db_read(struct db_context * ctx, struct db_record ** rec) {
  if(ctx->fd_read < 0) {
    perrorf("aodb: Database is not open for reading.\n");
    return -2;
  }

  struct db_record * newrec = malloc(sizeof(struct db_record)), * newrec_re;
  int nread = 0, rv, cur_pos = lseek(ctx->fd_read, 0, SEEK_CUR);

  while(nread < sizeof(struct db_record) && 0 < (rv = read(ctx->fd_read, ((char*)newrec) + nread, sizeof(struct db_record) - nread)))
    nread += rv;

  if(rv == -1) {
    perrorstrf("aodb: Error during record read: %s.\n");
    free(newrec);
    perrorf("aodb: Warning: partial read of record header: got %d, expected %ld. Rewound to byte %ld.\n",
            nread, sizeof(struct db_record), lseek(ctx->fd_read, cur_pos, SEEK_SET));
    return -2;
  } else if(nread < sizeof(struct db_record) && nread > 0) {
    free(newrec);
    perrorf("aodb: Warning: partial read of record header: got %d, expected %ld. Rewound to byte %ld.\n",
            nread, sizeof(struct db_record), lseek(ctx->fd_read, cur_pos, SEEK_SET));
    return -2;
  } else if(nread == 0) {
    free(newrec);
    return -1;
  }

  if(newrec->magic != AODB_MAGIC) {
    perrorf("aodb: Warning: bad magic number on record header: %x, length %d. Rewound to byte %ld.\n", newrec->magic,
            newrec->length, lseek(ctx->fd_read, cur_pos, SEEK_SET));
    return -2;
  }

  newrec_re = realloc(newrec, sizeof(struct db_record) + newrec->length);
  if(newrec_re == NULL) {
    free(newrec);
    perrorf("aodb: Warning: Could not realloc() space to read record. Rewound to byte %ld.\n",
            lseek(ctx->fd_read, -sizeof(struct db_record), SEEK_CUR));
    return -2;
  }
  nread = 0;
  while(nread < newrec_re->length && 0 < (rv = read(ctx->fd_read, newrec_re->end + nread, newrec_re->length - nread)))
    nread += rv;

  if(rv == -1) {
    perrorstrf("aodb: Error during record read: %s.\n");
    perrorf("aodb: Warning: partial read of record data: got %d, expected %d. Rewound to byte %ld.\n",
            nread, newrec_re->length, lseek(ctx->fd_read, cur_pos, SEEK_SET));
    free(newrec_re);
    return -2;
  } else if(nread < newrec_re->length) {
    perrorf("aodb: Warning: partial read of record data: got %d, expected %ld. Rewound to byte %ld.\n",
            nread, sizeof(struct db_record), lseek(ctx->fd_read, cur_pos, SEEK_SET));
    free(newrec_re);
    return -2;
  }

  newrec_re->data_ptr = newrec_re->end;
  *rec = newrec_re;
  return 0;
}

int db_write(struct db_context * ctx, const char * data, int length) {
  if(ctx->fd_append < 0) {
    perrorf("aodb: Database is not open for writing.\n");
    return -2;
  }

  struct db_record * newrec = malloc(sizeof(struct db_record) + length);
  if(newrec == NULL) {
    perrorf("aodb: Warning: Could not allocate memory for record write.\n");
    return -2;
  }

  memset(newrec, 0, sizeof(struct db_record));
  newrec->magic = AODB_MAGIC;

  struct timespec ts;
  int rv = clock_gettime(CLOCK_REALTIME, &ts);
  if(rv) {
    perrorstrf("aodb: Warning: Could not generate timestamp for write: %s.\n");
    free(newrec);
    return -2;
  }
  newrec->tv_sec = ts.tv_sec;
  newrec->tv_nsec = ts.tv_nsec;

  memcpy(newrec->end, data, length);
  newrec->length = length;

  int nwritten = -(int)sizeof(struct db_record);
  while(nwritten < length && 0 <= (rv = write(ctx->fd_append, ((char*)newrec) + nwritten + sizeof(struct db_record), length - nwritten)))
    nwritten += rv;

  if(rv < 0) {
    perrorstrf("aodb: Error during record write: %s\n");
    perrorf("aodb: Warning: partial write of record data: wrote %ld of %ld bytes.\n",
            nwritten + sizeof(struct db_record), length + sizeof(struct db_record));
    return -2;
  }

  free(newrec);
  return 0;
}

#ifdef TEST
int main() {
  struct db_context ctx;
  printf("db_init: %d\n", db_init(&ctx, "test.db"));
  struct db_record * rec;
  int rv, recs=0;
  while(!(rv = db_read(&ctx, &rec))) {
    printf("Record: %s\n", rec->data_ptr);
    ++recs;
  }
  char bfr[256];
  snprintf(bfr, 256, "This is a test! Record number %d.", recs);

  printf("db_write: %d\n", db_write(&ctx, bfr, strlen(bfr)+1));
  return 0;
}
#endif

