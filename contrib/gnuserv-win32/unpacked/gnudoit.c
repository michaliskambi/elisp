/* -*-C-*-
 Client code to locally and remotely evaluate lisp forms using GNU Emacs.

 This file is part of GNU Emacs.

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com).

 Please mail bugs and suggestions to the author at the above address.
*/

/*
 * This file incorporates new features added by Bob Weiner <weiner@mot.com>,
 * Darrell Kindred <dkindred@cmu.edu> and Arup Mukherjee <arup@cmu.edu>.
 * Please see the note at the end of the README file for details.
 *
 * (If gnuserv came bundled with your emacs, the README file is probably
 * ../etc/gnuserv.README relative to the directory containing this file)
 */

static char rcsid [] = "$Header: gnudoit.c,v 2.0 94/08/14 12:35:44 arup Exp $";

#include "gnuserv.h"

#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && !defined(INTERNET_DOMAIN_SOCKETS)
main ()
{
  fprintf (stderr,"Sorry, the Emacs server is only supported on systems that have\n");
  fprintf (stderr,"Unix Domain sockets, Internet Domain sockets or System V IPC.\n");
  exit (1);
} /* main */
#else /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */

main(argc,argv)
     int argc;
     char *argv[];
{
  int qflg = 0;					/* don't wait around for 
						 * gnu emacs to eval cmd */
  int errflg = 0;				/* option error */
  int c;					/* char from getopt */
  int s;					/* socket / msqid to server */
  int connect_type;           			/* CONN_UNIX, CONN_INTERNET, or
						 * CONN_IPC */
#ifdef INTERNET_DOMAIN_SOCKETS
  char *hostarg = NULL; 			/* remote hostname argument */
  u_short portarg = 0;				/* port number */
#endif /* INTERNET_DOMAIN_SOCKETS */
#ifdef SYSV_IPC
  struct msgbuf *msgp;				/* message */
#endif /* SYSV_IPC */

  progname = argv[0];
#if defined( WIN32 )
  WinsockInit();
#endif

  while ((c = getopt(argc, argv,
#ifdef INTERNET_DOMAIN_SOCKETS
		     "qh:p:"
#else /* !INTERNET_DOMAIN_SOCKETS */
		     "q"
#endif /* !INTERNET_DOMAIN_SOCKETS */
		     )) != EOF)
    switch (c) {
#ifdef INTERNET_DOMAIN_SOCKETS
    case 'h':					/* host name specified */
      hostarg = optarg;
      break;
    case 'p':					/* port number specified */
      portarg = atoi(optarg);
      break;
#endif /* INTERNET_DOMAIN_SOCKETS */
    case 'q':					/* quick mode specified */
      qflg++;
      break;
    case '?':
      errflg++;
    }; /* switch */

  if (errflg) {
    fprintf(stderr,
#ifdef INTERNET_DOMAIN_SOCKETS
	    "usage: %s [-q] [-h hostname] [-p port] [sexpr]...\n",
#else /* !INTERNET_DOMAIN_SOCKETS */
	    "usage: %s [-q] [sexpr]...\n",
#endif /* !INTERNET_DOMAIN_SOCKETS */
	    progname);
    exit (1);
  }; /* if */

#ifdef INTERNET_DOMAIN_SOCKETS
  connect_type = make_connection(hostarg, portarg, &s);
#else
  connect_type = make_connection(NULL, (u_short) 0, &s);
#endif

#ifdef SYSV_IPC
  if ((msgp = (struct msgbuf *) 
              malloc(sizeof *msgp + GSERV_BUFSZ)) == NULL) {
    fprintf(stderr,"%s: not enough memory for message buffer\n",progname);
    exit(1);
  }; /* if */

  msgp->mtext[0] = '\0';			/* ready for later strcats */
#endif /* SYSV_IPC */

  if (qflg) {
    send_string(s,"(server-eval-quickly '(progn ");
  }
  else {
    send_string(s,"(server-eval '(progn ");
  };

  if (optind < argc) {
    for (; optind < argc; optind++) {
      send_string(s, argv[optind]);
      send_string(s, " ");
    }
  } else {
    /* no sexp on the command line, so read it from stdin */
    int nb;
    char buf[GSERV_BUFSZ];
    while ((nb = read(fileno(stdin), buf, GSERV_BUFSZ-1)) > 0) {
      buf[nb] = '\0';
      send_string(s, buf);
    }
  }
  send_string(s,"))");

#ifdef SYSV_IPC
  if (connect_type == (int) CONN_IPC)
    disconnect_from_ipc_server(s,msgp,!qflg);
  else
#else /* !SYSV_IPC */
    disconnect_from_server(s,!qflg);
#endif /* !SYSV_IPC */

#if defined( WIN32 )
	WSACleanup();
#endif
  exit(0);

} /* main */

#endif /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */
