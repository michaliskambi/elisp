/* -*-C-*-

 Header file for the GNU Emacs server and client C code.

 This file is part of GNU Emacs.

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com), based on 
         'etc/server.c' and 'etc/emacsclient.c' from the 18.52 GNU
         Emacs distribution.

 Please mail bugs and suggestions to the author at the above address.
*/

/* HISTORY 
 * 11-Nov-1990		bristor@simba	
 *    Added EOT stuff.
 */

/*
 * This file incorporates new features added by Bob Weiner <weiner@mot.com>,
 * Darrell Kindred <dkindred@cmu.edu> and Arup Mukherjee <arup@cmu.edu>.
 * Please see the note at the end of the README file for details.
 *
 * (If gnuserv came bundled with your emacs, the README file is probably
 * ../etc/gnuserv.README relative to the directory containing this file)
 */

static char header_rcsid [] = "$Header: gnuserv.h,v 2.3 94/09/08 17:18:10 arup Exp $";

#define NO_SHORTNAMES

#define PATCHLEVEL 2

#define NO_SHORTNAMES
/* XXX change to "../src/config.h" if included with emacs */
#include "config.h"
#undef read
#undef write
#undef open
#undef close
#undef signal

/* Define the communication method between server and clients:
 *   You can have either or both kinds of sockets, but you can't mix
 *   sockets with sysv ipc
 */


#define INTERNET_DOMAIN_SOCKETS
/* #define UNIX_DOMAIN_SOCKETS */
/* #define SYSV_IPC  */

#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && !defined(INTERNET_DOMAIN_SOCKETS)

 #ifdef HAVE_SYSVIPC
  #define SYSV_IPC		/* SYSV systems use SYSV IPC by default */
 #endif /* HAVE_SYSVIPC */

 #ifdef BSD
  #define UNIX_DOMAIN_SOCKETS	/* BSD systems use Unix Domain sockets by default */
 #endif /* BSD */

#endif /* No communication method pre-defined */

#include <sys/types.h>
#if ! defined( WIN32 )
 #include <sys/param.h>
#else
 #include <io.h>
 #include <process.h>
#endif
#include <stdio.h>
#ifdef STDC_HEADERS
 #include <stdlib.h>
 #include <string.h>
#endif
#include <signal.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H
 #include <unistd.h>
#endif

#ifdef HAVE_SYS_TIME_H
 #include <sys/time.h>
#endif

/*
 * If you are using SYSV_IPC, you might want to make the buffer size bigger
 * since it limits the size of requests and responses. Don't make it bigger
 * than your system's max message size though (usually a couple of k) or else
 * msgsend will start failing. For sockets, using the system BUFSIZ is usually
 * what you want. 
 */

# define GSERV_BUFSZ BUFSIZ


#ifdef SYSV_IPC
 #include <sys/ipc.h>
 #include <sys/msg.h>

 #define send_string(s,str) \
  if (strlen(msgp->mtext) + strlen(str) < GSERV_BUFSZ) \
     strcat(msgp->mtext,str); \
  else \
  { \
    fprintf(stderr,"%s: not enough message buffer space\n",progname); \
     exit(1); \
  } \

#endif /* SYSV_IPC */

#if defined(INTERNET_DOMAIN_SOCKETS) || defined(UNIX_DOMAIN_SOCKETS)
 #if ! defined( WIN32 )
  #include <sys/socket.h>
 #endif
#endif /* INTERNET_DOMAIN_SOCKETS || UNIX_DOMAIN_SOCKETS */

#ifdef INTERNET_DOMAIN_SOCKETS
 #if ! defined( WIN32 )
  #include <netdb.h>
  #include <netinet/in.h>
  #include <arpa/inet.h>
 #else
  #include <winsock2.h>
 #endif
 #define TABLE_SIZE 101		/* The number of entries in the hash table */
 #define HASH(host) host		/* Rather simplistic hash function */
 #define DEFAULT_PORT 21490	/* default port number to use is
							 * DEFAULT_PORT + uid */
#endif /* INTERNET_DOMAIN_SOCKETS */

#ifdef UNIX_DOMAIN_SOCKETS
 #include <sys/un.h>
 #define HIDE_UNIX_SOCKET	/* put the unix socket in a protected dir */
#endif /* UNIX_DOMAIN_SOCKETS */

/* On some platforms, we need to do the equivalent of "stty litout" to get
 * characters like ^D to pass through to emacs.  This problem has only
 * been observed under emacs18; fsf19 and lemacs are probably okay without it.
 */
#ifndef DONT_USE_LITOUT
 #if !defined(HAVE_TERMIO) && !defined(HAVE_TERMIOS) && !defined(VMS)
  #if !defined(MSDOS) && !defined(BSD4_1)
   #define USE_LITOUT
  #endif
 #endif
#endif


#define HOSTNAMSZ 255		/* max size of a hostname */
#define REPLYSIZ 300		/* max size of reply from server to client */
#undef FALSE
#define FALSE 0
#undef TRUE
#define TRUE 1

extern char *getenv();
extern char *optarg;
extern int optind;
extern char *progname;

#ifndef BSD
 extern char *getcwd();
#endif

#define max2(x,y) (((x) > (y)) ? (x) : (y))
#define min2(x,y) (((x) < (y)) ? (x) : (y))

#ifndef _NFILE            /* rough guess at maximum number of open files */
 #define _NFILE 20
#endif

#define EOT_STR "\004"
#define EOT_CHR '\004'

/* connection types */
#define CONN_UNIX     0
#define CONN_INTERNET 1
#define CONN_IPC      2

#if defined( _USE_PROTOTYPES )

 int connect_to_internet_server( char *serverhost, int port );
 int make_connection( char *hostarg, int portarg, int *s );
 void send_string( int s, char *msg );
 void disconnect_from_server( int s, int echo );
 int internet_addr( char *host );
 
 #if defined( WIN32 )
  void WinsockInit(void);
  BOOL StartEmacs(void);
  #include <windows.h>
  #include <ctype.h>
 #endif
#endif

#include "getopt.h"

#if ! defined _LOCAL_HOST_ACCESS
 #define _LOCAL_HOST_ACCESS 0
#endif

