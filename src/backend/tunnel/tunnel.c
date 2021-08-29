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

/**
 * Code mainly adopted from: http://www.libssh2.org/examples/ssh2_exec.html.
 */

/*
 * This program here is mainly written as Racket does not
 * seem to work nicely with blocking I/O (select) in C,
 * short of starting a pthread and using pipes for communications.
 * Instead, we farm out the work of tunnel management to an external
 * program, and use Racket's nice nonblocking process I/O to communicate
 * with it.
 *
 */
#include <openssl/opensslv.h>
#include <seashell-config.h>
#include <libssh2.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/types.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>

/** These really ought to be configurable.  Oh well.  Might want
 *  to put these in the build system's autogenerated header. */
#define HOSTS_FILE (INSTALL_PREFIX "/etc/seashell_hosts")
#define DEBUG_HOSTS_FILE (BUILD_DIR "/etc/seashell_hosts")
#define DUMP_FILE  "/tmp/seashell_hosts"
#if SEASHELL_DEBUG
#define FPRINTF_IF_DEBUG(...) fprintf(__VA_ARGS__)
#else
#define FPRINTF_IF_DEBUG(...)
#endif


#define SET_ERROR(x) do{if(error){*error = (x);}}while(0)
#define TUNNEL_ERROR_RESOLV 1
#define TUNNEL_ERROR_CONNECT 2
#define TUNNEL_ERROR_SESSION_START 3
#define TUNNEL_ERROR_SESSION_HANDSHAKE 4
#define TUNNEL_ERROR_HOSTS_FILE 5
#define TUNNEL_ERROR_HOST 6
#define TUNNEL_ERROR_CREDS 7
#define TUNNEL_ERROR_CHANNEL_OPEN 8
#define TUNNEL_ERROR_LAUNCH_SEASHELL 9
#define IO_ERROR 10
#define OTHER_ERROR 11

#define MAX(a, b) ((a) < (b) ? (b) : (a))

struct seashell_connection {
  int sockfd;
  LIBSSH2_SESSION* session;
  LIBSSH2_CHANNEL* channel;
};


/**
 * Callback for libssh2_userauth_keyboard_interactive
 * password_for_kbd_callback must be set before this is called.
 */
char *password_for_kbd_callback = NULL;
static void kbd_callback(const char *name, int name_len,
                         const char *instruction, int instruction_len,
                         int num_prompts,
                         const LIBSSH2_USERAUTH_KBDINT_PROMPT *prompts,
                         LIBSSH2_USERAUTH_KBDINT_RESPONSE *responses,
                         void **abstract)
{
    (void)name;
    (void)name_len;
    (void)instruction;
    (void)instruction_len;
    if(num_prompts == 1) {
        responses[0].text = strdup(password_for_kbd_callback);
        responses[0].length = strlen(password_for_kbd_callback);
    }
    (void)prompts;
    (void)abstract;
} /* kbd_callback */


/**
 * seashell_tunnel_{startup/teardown} (void)
 * Sets up/tears down the operating environment for the tunneling code.
 *
 * Returns: 0 on success, negative values otherwise.
 * Consult libssh2_init/exit(3).
 */
int seashell_tunnel_startup (void) {
  return libssh2_init(0);
}
void seashell_tunnel_teardown (void) {
  libssh2_exit();
}

/**
 * seashell_tunnel_connect_password (const char* host, const char* user, const char* password, int* error)
 * Connects to the host via SSH on port 22, and launches a Seashell backend instance for that user
 * on the host.
 *
 * Consults /etc/seashell_hosts for host's SSH public keys.  If this file does not exist,
 * this function will fail for security reasons.  /etc/seashell_hosts is a standard
 * OpenSSH known_hosts file.
 *
 * Arguments:
 *  host - Host to connect to.
 *  user - User to run as.
 *  password - User's password.
 *  error - [optional] denotes error on failure.
 *  remote_addr - Address to which the remote IP address will
 *   be written. Reserve 128 bytes.
 *  family - Address family.
 *  target - Target to execute.
 *
 * Returns:
 *  Handle to connection object on success, NULL otherwise.
 *  If error is NOT null, error will hold more detailed error information.
 */
struct seashell_connection* seashell_tunnel_connect_password (const char* host,
    const char* user,
    const char* password,
    int* error,
    char * remote_addr,
    int* family,
    char* target) {
  struct addrinfo hints;
  struct addrinfo *results, *rp;
  int sockfd;
  int i, e;
  struct seashell_connection* result = NULL;

  /* Resolve the host's address.
   * See getaddrinfo(3) for how this works.
   */
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = 0;
  hints.ai_protocol = 0;

  e = getaddrinfo(host, "22", &hints, &results);
  if (e != 0) {
    SET_ERROR(TUNNEL_ERROR_RESOLV);
    return NULL;
  }

  for (rp = results; rp != NULL; rp = rp->ai_next) {
    sockfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (sockfd == -1)
      continue;

    if (connect(sockfd, rp->ai_addr, rp->ai_addrlen) != -1)
      break;

    close(sockfd);
  }

  /* Write address that we're connecting to into
   * remote_addr.
   */

  if(rp != NULL) {
    *family = rp->ai_family;

    switch(rp->ai_family) {
      case AF_INET:
        if(inet_ntop(rp->ai_family, &((struct sockaddr_in *)rp->ai_addr)->sin_addr, remote_addr, 128) == NULL) {
          SET_ERROR(TUNNEL_ERROR_RESOLV);
          return NULL;
        }
        break;
      case AF_INET6:
        if(inet_ntop(rp->ai_family, &((struct sockaddr_in6 *)rp->ai_addr)->sin6_addr, remote_addr, 128) == NULL) {
          SET_ERROR(TUNNEL_ERROR_RESOLV);
          return NULL;
        }
        break;
      default:
        SET_ERROR(TUNNEL_ERROR_RESOLV);
        return NULL;
    }
  }

  freeaddrinfo(results);

  /* Either rp == NULL, in which case we failed at connecting,
   * or sockfd holds our socket.
   */
  if (rp == NULL) {
    SET_ERROR(TUNNEL_ERROR_CONNECT);
    return NULL;
  }

  /** Set up the session */
  LIBSSH2_SESSION* session;
  LIBSSH2_CHANNEL* channel;
  LIBSSH2_KNOWNHOSTS* hosts;
  size_t len;
  int type;

  session = libssh2_session_init();
  if (!session) {
    SET_ERROR(TUNNEL_ERROR_SESSION_START);
    goto session_teardown;
  }

  e = libssh2_session_handshake(session, sockfd);
  if (e) {
    SET_ERROR(TUNNEL_ERROR_SESSION_HANDSHAKE);
    goto session_teardown;
  }

  hosts = libssh2_knownhost_init(session);
  if (!hosts) {
    SET_ERROR(TUNNEL_ERROR_HOSTS_FILE);
    goto session_teardown;
  }

  if (!IS_INSTALLED() && access(DEBUG_HOSTS_FILE, F_OK) != -1) {
    libssh2_knownhost_readfile(hosts, DEBUG_HOSTS_FILE, LIBSSH2_KNOWNHOST_FILE_OPENSSH);
  } else {
    libssh2_knownhost_readfile(hosts, HOSTS_FILE, LIBSSH2_KNOWNHOST_FILE_OPENSSH);
  }

  const char* fingerprint = libssh2_session_hostkey(session, &len, &type);
  if (!fingerprint || type == LIBSSH2_HOSTKEY_TYPE_UNKNOWN) {
    libssh2_knownhost_free(hosts);

    SET_ERROR(TUNNEL_ERROR_HOST);
    goto session_teardown;
  }

  struct libssh2_knownhost *hostkey;
  /** NOTE: Documentation is buggy.  hostkey MUST be passed. */
  int check = libssh2_knownhost_check(hosts, host, fingerprint, len,
      LIBSSH2_KNOWNHOST_TYPE_PLAIN | LIBSSH2_KNOWNHOST_KEYENC_RAW, &hostkey);

  if (check != LIBSSH2_KNOWNHOST_CHECK_MATCH) {
    fprintf(stderr, "libssh2_knownhost_check returned: %d\n", check);
    fprintf(stderr, "type is: %d\n", type);

    int keytype = 0;

    switch (type) {
      case LIBSSH2_HOSTKEY_TYPE_RSA:
        keytype = LIBSSH2_KNOWNHOST_KEY_SSHRSA;
        break;
      case LIBSSH2_HOSTKEY_TYPE_DSS:
        keytype = LIBSSH2_KNOWNHOST_KEY_SSHRSA;
        break;
    }

    if (keytype) {
      libssh2_knownhost_addc(hosts, host, NULL, fingerprint, len,
          "Generated from Seashell Tunnel", strlen("Generated from Seashell Tunnel"),
            LIBSSH2_KNOWNHOST_TYPE_PLAIN | LIBSSH2_KNOWNHOST_KEYENC_RAW
          | (type == LIBSSH2_HOSTKEY_TYPE_RSA ? LIBSSH2_KNOWNHOST_KEY_SSHRSA : LIBSSH2_KNOWNHOST_KEY_SSHDSS),
          NULL);

      libssh2_knownhost_writefile(hosts, DUMP_FILE, LIBSSH2_KNOWNHOST_FILE_OPENSSH);
      fprintf(stderr, "%s: Check SSH key for %s! Keys written to %s\n", user, host, DUMP_FILE);
    } else {
      fprintf(stderr, "%s: Check SSH key for %s!\n", user, host, DUMP_FILE);
      fprintf(stderr, "%s: Keys not written to file - contact Seashell Maintainers to add support for the LibSSH2 key format %d\n", user, type);
    }

    libssh2_knownhost_free(hosts);

    SET_ERROR(TUNNEL_ERROR_HOST);
    goto session_teardown;
  }
  libssh2_knownhost_free(hosts);

  FPRINTF_IF_DEBUG(stderr, "%s: Host check passed for %s (fingerprint type %d) - ", user, host, type);
  for(i = 0; i < 20; i++) {
    FPRINTF_IF_DEBUG(stderr, "%02X ", (unsigned char)fingerprint[i]);
  }
  FPRINTF_IF_DEBUG(stderr, "\n");

  // SSH in using keyboard-interactive method
  password_for_kbd_callback = strdup(password);
  if(password_for_kbd_callback) {
    FPRINTF_IF_DEBUG(stderr, "%s: Trying keyboard interactive SSH authentication\n", user);
    while((e = libssh2_userauth_keyboard_interactive(session, user, &kbd_callback)) == LIBSSH2_ERROR_EAGAIN);
    if(password_for_kbd_callback) {
      free(password_for_kbd_callback);
      password_for_kbd_callback = NULL;
    }
  } else {
    FPRINTF_IF_DEBUG(stderr, "%s: Error connecting: No more memory!\n", user);
    SET_ERROR(TUNNEL_ERROR_CONNECT);
    goto session_teardown;
  }

  if (e) {
    FPRINTF_IF_DEBUG(stderr, "%s: Error authenticating: %d\n", user, e);
    SET_ERROR(TUNNEL_ERROR_CREDS);
    goto session_teardown;
  }

  channel = libssh2_channel_open_session(session);
  if (!channel) {
    SET_ERROR(TUNNEL_ERROR_CHANNEL_OPEN);
    goto session_teardown;
  }

  /**
   * Ideally we'd have a subsystem configured,
   * as I don't see a good way of pulling out of ssh2
   * if the target does not exist.
   */
  e = libssh2_channel_exec(channel, target);
  if (e) {
    SET_ERROR(TUNNEL_ERROR_LAUNCH_SEASHELL);
    goto channel_teardown;
  }

  result = malloc(sizeof(struct seashell_connection));
  if (!result) {
    SET_ERROR(TUNNEL_ERROR_SESSION_START);
    goto channel_teardown;
  }

  result->sockfd = sockfd;
  result->session = session;
  result->channel = channel;

  goto end;
channel_teardown:
  libssh2_channel_free(channel);
session_teardown:
  libssh2_session_free(session);
  close(sockfd);
end:
  return result;
}

/**
 * seashell_set_{non}blocking (struct seashell_connection* conn)
 * Puts the connection in {non}blocking mode.
 *
 * Arguments:
 *  conn - Connection to set in nonblocking mode.
 */
void seashell_set_nonblocking (struct seashell_connection* conn) {
  libssh2_session_set_blocking(conn->session, 0);
}
void seashell_set_blocking (struct seashell_connection* conn) {
  libssh2_session_set_blocking(conn->session, 1);
}

/**
 * seashell_tunnel_free (struct seashell_connection* conn)
 * Closes the tunnel connection.
 *
 * Arguments:
 *  conn - Connection to close.
 */
void seashell_tunnel_free (struct seashell_connection* conn) {
  if (conn) {
    libssh2_channel_free(conn->channel);
    libssh2_session_disconnect(conn->session, "Seashell's done!");
    libssh2_session_free(conn->session);
    close(conn->sockfd);
    free(conn);
  }
}

/**
 * loop_and_copy (int infd, int outfd, struct seashell_connection* conn)
 * Copies data to/from the file descriptors and socket.
 *
 *
 * Arguments:
 *  infd, outfd - input/output file descriptors.
 *  conn - Seashell connection object.
 *
 * Returns:
 *  0 on clean disconnect, nonzero value otherwise.
 */
int loop_and_copy(int infd, int outfd, struct seashell_connection* conn) {
  struct timeval timeout;
  fd_set readfds;

  char buffer[4096];
  ssize_t start = 0, len = 0, rc = 0;
  int nfds = 1 + MAX(infd, conn->sockfd);

  while (1) {
    FD_ZERO(&readfds);
    FD_SET(conn->sockfd, &readfds);
    FD_SET(infd, &readfds);

    timeout.tv_sec = 10;
    timeout.tv_usec = 0;

    if (select(nfds, &readfds, NULL, NULL, &timeout) > 0) {
      /** We're OK only when we have something to read from and something to write to. */

      /**
       *  Something to read from remote - might block on write, this is OK.
       *  Time permitting, it might be nice to have a write queue and
       *  go through the select loop again for fairness,
       *  but it really doesn't matter.
       */
      if (FD_ISSET(conn->sockfd, &readfds)) {
        /** Do our reads in nonblocking mode as we may get payload data for
         *  some other stream. */
        seashell_set_nonblocking(conn);
        len = libssh2_channel_read(conn->channel, buffer, sizeof(buffer));
        seashell_set_blocking(conn);

        if (len == LIBSSH2_ERROR_EAGAIN) {
        }
        else if (len < 0) {
          return len;
        } else if (len > 0) {

          start = 0;

          while (len) {
            rc = write(outfd, buffer + start, len);
            if (rc < 0) {
              return rc;
            }
            len -= rc;
            start += rc;
          }
        }

        /** Drain stderr. */
        seashell_set_nonblocking(conn);
        len = libssh2_channel_read_stderr(conn->channel, buffer, sizeof(buffer));
        seashell_set_blocking(conn);
        if (len == LIBSSH2_ERROR_EAGAIN) {
        }
        else if (len < 0) {
          return len;
        } else if (len > 0) {
          start = 0;

          while (len) {
            rc = write(2, buffer + start, len);
            if (rc < 0) {
              return rc;
            }
            len -= rc;
            start += rc;
          }
        }
      }

      /**
       * Something to read from local - might block on write, this is OK.
       * See above for rationale, and libssh2_channel_write doesn't seem
       * to block nicely.  Oh well.
       */
      if (FD_ISSET(infd, &readfds)) {
        len = read(infd, buffer, sizeof(buffer));

        if (len < 0) {
          return len;
        } else if (len == 0) {
          return 0;
        } else {
          start = 0;

          while (len) {
            rc = libssh2_channel_write(conn->channel, buffer + start, len);
            if (rc < 0) {
              return rc;
            }
            len -= rc;
            start += rc;
          }
        }
      }
    }
    /**
     * And if we died...
     */
    if (libssh2_channel_eof(conn->channel) == 1) {
      return 0;
    }
  }
}

/**
 * usage: seashell-tunnel [username] [host] [target]
 *
 * First n bytes on standard input should be:
 *
 * ----------------------------------------------------------------------------------------------------------
 * | length / 4 byte unsigned integer | authentication method / 1 byte | authentication data : length bytes |
 * ----------------------------------------------------------------------------------------------------------
 *
 * Authentication methods:
 *  0 - password.
 *
 * Currently, only password authentication is supported.  Therefore,
 * the authentication method is ignored.  It might be worth looking
 * at supporting public key authentication in the future.  This'll
 * require extending the connection launcher.
 *
 * Bytes after n are simply forwarded onwards with no processing applied.
 *
 * For password authentication, the null terminating byte is expected to be in the authentication data.
 *
 * seashell-tunnel will write a single ASCII 'O' (79) denoting handshake success before starting
 * two-way forwarding.
 *
 * IP address are also written; IPv4 passed as X.X.X.X, IPv6 as [:X:X::X]
 */
int main (int argc, char *argv[]) {
  uint32_t length = 0;
  uint8_t method = 0;
  int8_t* data = NULL;
  int i = 0;
  char stderr_buffer[4096];
  ssize_t stderr_read = 0;

  if (argc < 4) {
    fprintf(stderr, "usage: %s [username] [host] [target]\n", argv[0]);
    return IO_ERROR;
  }

  FPRINTF_IF_DEBUG(stderr, "%s: Launching tunnel!\n", argv[1]);

  FPRINTF_IF_DEBUG(stderr, "OPENSSL_VERSION_TEXT: %s\n", OPENSSL_VERSION_TEXT);
  FPRINTF_IF_DEBUG(stderr, "OPENSSL_VERSION_NUMBER: %lx\n", OPENSSL_VERSION_NUMBER);
  FPRINTF_IF_DEBUG(stderr, "libssh2 version: %s\n", libssh2_version(0));

  for (i = 0; i < 4; i++) {
    uint8_t buf;
    if (1 != read(0, &buf, 1)) {
      fprintf(stderr, "%s: I/O error on reading authentication packet length.\n", argv[1]);
      return IO_ERROR;
    }
    length |= buf << (8 * i);
  }
  FPRINTF_IF_DEBUG(stderr, "%s: Read authentication packet length.\n", argv[1]);

  if (1 != read(0, &method, 1)) {
    fprintf(stderr, "%s: I/O error on reading authentication method.\n", argv[1]);
    return IO_ERROR;
  }
  FPRINTF_IF_DEBUG(stderr, "%s: Read authentication method.\n", argv[1]);

  data = malloc(length);
  if (!data) {
    fprintf(stderr, "%s: Ran out of memory!\n", argv[1]);
    return OTHER_ERROR;
  }
  if (length != read(0, data, length)) {
    fprintf(stderr, "%s: Couldn't read authentication data!\n", argv[1]);
    return IO_ERROR;
  }
  FPRINTF_IF_DEBUG(stderr, "%s: Read authentication data.\n", argv[1]);

  /** This is where we need to handle extra authentication methods. */
  int error; char* exitsignal;

  error = seashell_tunnel_startup();
  if (error) {
    fprintf(stderr, "%s: Error launching libssh2!\n", argv[1]);
    goto end;
  }

  char remote_addr[128];
  memset(remote_addr, 0, 128);
  int family;

  struct seashell_connection* conn = seashell_tunnel_connect_password(
      argv[2], argv[1], data, &error, remote_addr, &family, argv[3]);

  if (!conn) {
    fprintf(stderr, "%s: Error on opening tunnel to %s: %d\n", argv[1], argv[2], error);
    goto end;
  }

  /** Signal success */
  {
    const int8_t success = 'O';
    write(1, &success, 1);
    uint8_t addrlen = strlen(remote_addr);

    /** Write the address, formatted for URI safety. */

    /** IPv6 addresses need to be formatted for safety. */
    if (family == AF_INET6) {
      char buffer[130] = {0};
      int bufferlen = snprintf(buffer, 130, "[%s]", remote_addr);
          
      write(1, &bufferlen, 1);
      write(1, buffer, bufferlen);
    }
    /** IPv4 addresses do not need to be formatted. */
    else if (family == AF_INET) {
      write(1, &addrlen, 1);
      write(1, remote_addr, addrlen);
    } else {
      fprintf(stderr, "%s: Unknown address family %d!\n", argv[1], family);
      goto end;
    }

    FPRINTF_IF_DEBUG(stderr, "%s: Remote address is '%s' (%d) (%d)\n", argv[1], remote_addr, (int)addrlen, family);
  }
  FPRINTF_IF_DEBUG(stderr, "%s: Tunnel launched!\n", argv[1]);

  /** Now we select from fd 0, write to socket,
   *  select from socket, write to fd 1 */
  error = loop_and_copy(0, 1, conn);

  if (error) {
    fprintf(stderr, "%s: I/O error on copy: %d\n", argv[1], error);
    goto report_errors;
  }

  /** Make sure the remote end hung up cleanly. */
  error = libssh2_channel_get_exit_status(conn->channel);
  libssh2_channel_get_exit_signal(conn->channel, &exitsignal,
      NULL, NULL, NULL, NULL, NULL);

  FPRINTF_IF_DEBUG(stderr, "%s: Remote end hung up with %d (%s)\n", argv[1], error, exitsignal);

  if (exitsignal) {
    error = OTHER_ERROR;
  }
report_errors:
  /** Drain stderr on the SSH connection. */
  while ((stderr_read = libssh2_channel_read_stderr(conn->channel, stderr_buffer, sizeof(char)*4096)) > 0) {
    fwrite(stderr_buffer, sizeof(char), stderr_read, stderr);
  }
end:
  seashell_tunnel_free(conn);
  seashell_tunnel_teardown();
  return error;
}
