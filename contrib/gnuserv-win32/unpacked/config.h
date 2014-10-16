/* This file will do as a replacement for config.h for the purposes of
 * gnuserv if you don't have access to the config.h with which your emacs 
 * was built.  Edit it as necessary and rename it to "config.h".
 */

/* On some platforms, we need to do the equivalent of "stty litout" to get
 * characters like ^D to pass through to emacs.  This problem has only
 * been observed under emacs18; fsf19 and lemacs are probably okay without it.
 * 
 * If you have a system which provides <sgtty.h> (or <bsd/sgtty.h> for linux),
 * you should probably define USE_LITOUT.  Otherwise, define DONT_USE_LITOUT.
 *
 * Use exactly one of the following:
 */
/* #define USE_LITOUT */
#define DONT_USE_LITOUT


/* AIX needs to pick up sys/select.h for fd_set and friends. */
#ifdef _AIX
 #define AIX
#endif

#if defined( WIN32 )

 #define MAXPATHLEN _MAX_PATH
 #define STDC_HEADERS
 #define _USE_PROTOTYPES

 #define bzero( a, b ) memset( a, 0, b )

 /* This makes sense on NT, but probably not on a true multi user system */
 #define _LOCAL_HOST_ACCESS 1
 /* Winsock doesn't define write and read to sockets :-( */
 #define _WANT_SEND_RECV 1

#endif
