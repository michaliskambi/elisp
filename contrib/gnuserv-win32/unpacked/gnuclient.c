/* -*-C-*-
 Client code to allow local and remote editing of files by GNU Emacs.

 This file is part of GNU Emacs. 

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com), based on 
         'etc/emacsclient.c' from the GNU Emacs 18.52 distribution.

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

static char rcsid [] = "$Header: gnuclient.c,v 2.0 94/08/14 12:34:43 arup Exp $";

#include "gnuserv.h"

#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && \
    !defined(INTERNET_DOMAIN_SOCKETS)
main ()
{
  fprintf (stderr,"Sorry, the Emacs server is only supported on systems that have\n");
  fprintf (stderr,"Unix Domain sockets, Internet Domain sockets or System V IPC.\n");
  exit (1);
} /* main */
#else /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */

static char cwd[MAXPATHLEN+2];			/* current working directory when calculated */
static char *cp = NULL;				/* ptr into valid bit of cwd above */

#if defined( WIN32 )

#define FORWARD_SLASH		'/'
#define BACKWARD_SLASH		'\\'

// filename_expand -- try to convert the given filename into a 
// fully-qualified pathname.
//
// 'fullpath' - points to buffer that will be filled with full path
// 'filename' - file to find path of

BOOL filename_expand ( char *fullpath, char *filename )
{
	char *fname;
	char *p = 0;
	WIN32_FIND_DATA finddata;
	HANDLE handle;

	if (!GetFullPathName (filename, MAXPATHLEN + 2, fullpath, &fname)) {
		fprintf (stderr, "Unable to get full pathname for %s\n", filename);
		return (FALSE);
		}

	// Make sure that the drive letter is lower case.
	if (fullpath[1] == ':' && isupper (fullpath[0])) {
		fullpath[0] = (char)tolower (fullpath[0]);
		}

    // Convert all backslashes to forward-slashes
    for ( p = fullpath; *p; p++) {
		if (BACKWARD_SLASH == *p)
			*p = FORWARD_SLASH;
		}

	// Since GetFullPathName doesn't always seem to return the long
	// filename (as documented) we get it ourselves.
	handle = FindFirstFile (filename, &finddata);
	if (handle != INVALID_HANDLE_VALUE) {
		strcpy (fname, finddata.cFileName);
		FindClose (handle);
		}

	return (TRUE);
}

void showEmacs( int unIconify )
{
  HWND hWnd = FindWindow ("Emacs", NULL);
  if ( hWnd ) {
    /* Is the Emacs window iconified ? */
    if (IsIconic (hWnd)) {
      if (unIconify) {
        ShowWindow (hWnd, SW_SHOWNORMAL);
        /* Need this since Emacs thinks it is still iconified otherwise! */
        SendMessage (hWnd, WM_SYSCOMMAND, SC_RESTORE, 0);
      }
    }
    else {
      SetForegroundWindow (hWnd);
    }
  }
  else
    StartEmacs();
}

#else

/*
  get_current_working_directory -- return the cwd.
*/
char *get_current_working_directory()
{
  if (cp == NULL) {				/* haven't calculated it yet */
#ifdef BSD
    if (getwd(cwd) == 0) {
#else /* !BSD */
    if (getcwd(cwd,MAXPATHLEN) == NULL) {
#endif /* !BSD */
      perror(progname);
      fprintf(stderr,"%s: unable to get current working directory\n",progname);
      exit(1);
    }; /* if */
    
    /* on some systems, cwd can look like '@machine/' ... */
    /* ignore everything before the first '/' */
    for (cp = cwd; *cp && *cp != '/'; ++cp)
      ;				           

  }; /* if */

  return cp;
    
} /* get_current_working_directory */

/*
  filename_expand -- try to convert the given filename into a fully-qualified
  		     pathname.
*/
void filename_expand(fullpath,filename)
     char *fullpath;				/* returned full pathname */
     char *filename;				/* filename to expand */
{
  int len;

  fullpath[0] = '\0';
  
  if(filename[0] && filename[0] != '/') {	/* relative filename */
    
    strcat(fullpath,get_current_working_directory());
    len = strlen(fullpath);
     
    if (len > 0 && fullpath[len-1] == '/')	/* trailing slash already? */
      ;						/* yep */
    else
      strcat(fullpath,"/");			/* nope, append trailing slash */
  }; /* if */

  strcat(fullpath,filename);

} /* filename_expand */

#endif

main(argc,argv)
     int argc;
     char *argv[];
{
  int starting_line = 1;			/* line to start editing at */
  char command[MAXPATHLEN+50];		/* emacs command buffer */
  char fullpath[MAXPATHLEN+1];		/* full pathname to file */
  int qflg = 0;						/* quick edit, don't wait for 
									 * user to finish */
  int errflg = 0;					/* option error */
  int c;							/* char from getopt */
  int s;							/* socket / msqid to server */
  int connect_type;           		/* CONN_UNIX, CONN_INTERNET, or
									 * CONN_IPC */
  int eflg = 0;                     /* evaluate flag */
#if defined( WIN32 )
  int wintofront = 0;				/* wintofront flag */
  int uniconify = 0;				/* uniconify flag */
  int runEmacs = 0;
#endif
#ifdef INTERNET_DOMAIN_SOCKETS
  char *hostarg = NULL;				/* remote hostname */
  char thishost[HOSTNAMSZ] = {0};	/* this hostname */
  char remotepath[MAXPATHLEN+1]={0};/* remote pathname */
  int rflg = 0;						/* pathname given on cmdline */
  u_short portarg = 0;				/* port to server */
  char *ptr;						/* return from getenv */
#endif /* INTERNET_DOMAIN_SOCKETS */
#ifdef SYSV_IPC
  struct msgbuf *msgp;				/* message */
#endif /* SYSV_IPC */

  progname = argv[0];
#if defined( WIN32 )
  WinsockInit();
#endif

#ifdef WIN_VERSION
	qflg++;
#endif

	while ((c = getopt(argc, argv,

#ifdef INTERNET_DOMAIN_SOCKETS
#if defined( WIN32 )
		     "h:p:r:qfFxe"
#else
			 "h:p:r:qe"
#endif
#else /* !INTERNET_DOMAIN_SOCKETS */
		     "qe"
#endif /* !INTERNET_DOMAIN_SOCKETS */

		     )) != EOF)
    switch (c) {
    case 'q':					/* quick mode specified */
      qflg++;
      break;
    case 'e':					/* eval sexpr */
      eflg++;
      break;

#ifdef INTERNET_DOMAIN_SOCKETS
    case 'h':				/* server host name specified */
      hostarg = optarg;
      break;
    case 'r':				/* remote path from server specifed */
      strcpy(remotepath,optarg);
      rflg++;
      break;
    case 'p':				/* port number specified */
      portarg = atoi(optarg);
      break;
#if defined( WIN32 )
    case 'f':
      wintofront = 1;
      break;
    case 'F':
      wintofront = 1;
      uniconify = 1;
      break;
    case 'x':				/* Just run and or show emacs  */
      runEmacs = 1;
      break;
#endif
#endif /* INTERNET_DOMAIN_SOCKETS */

    case '?':
      errflg++;
    }; /* switch */

  if (errflg) {
    fprintf(stderr,
#ifdef INTERNET_DOMAIN_SOCKETS
#if defined( WIN32 )
	    "usage: %s [-qfF] [-h host] [-p port] [-r pathname] [[+line] path] ...\n"
		"       or        [-qfF] [-h host] [-p port] -e [sexpr]\n"
		"       or        -x\n",
#else
	    "usage: %s [-q] [-h hostname] [-p port] [-r pathname] [[+line] path] ...\n"
		"       or        [-q] [-h host] [-p port] -e [sexpr]\n",
#endif
#else /* !INTERNET_DOMAIN_SOCKETS */
	    "usage: %s [-q] [[+line] path] ...\n"
		"       or        [-q] -e [sexpr]\n"

#endif /* !INTERNET_DOMAIN_SOCKETS */
	    progname);
    exit (1);
  }; /* if */

#if defined( WIN32 )
  if ( runEmacs ) {		/* just run and or show Emacs and then exit */
    showEmacs( 1 );
    exit(0);
  }
#endif

#ifdef INTERNET_DOMAIN_SOCKETS
  connect_type = make_connection(hostarg, portarg, &s);
#else
  connect_type = make_connection(NULL, (u_short) 0, &s);
#endif

#ifdef INTERNET_DOMAIN_SOCKETS
  if (connect_type == (int) CONN_INTERNET) {
    gethostname(thishost,HOSTNAMSZ);
    if(!rflg) {					/* attempt to generate a path 
						 * to this machine */
      if((ptr=getenv("GNU_NODE")) != NULL)	/* user specified a path */
	strcpy(remotepath,ptr);
    }
#if 0  /* This is really bogus... re-enable it if you must have it! */
#if defined(hp9000s300) || defined(hp9000s800)
    else if (strcmp(thishost,hostarg)) {	/* try /net/thishost */
      strcpy(remotepath,"/net/"); 		/* (this fails using internet 
						   addresses) */
      strcat(remotepath,thishost);
    }
#endif
#endif
  }  else {					/* same machines, no need for path */
      remotepath[0] = '\0';			/* default is the empty path */
  }
#endif /* INTERNET_DOMAIN_SOCKETS */

#ifdef SYSV_IPC
  if ((msgp = (struct msgbuf *) 
              malloc(sizeof *msgp + GSERV_BUFSZ)) == NULL) {
    fprintf(stderr,"%s: not enough memory for message buffer\n",progname);
    exit(1);
  }; /* if */

  msgp->mtext[0] = '\0';			/* ready for later strcats */
#endif /* SYSV_IPC */

  if (eflg) {
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
  }
  else {
    if (qflg) {
      send_string(s,"(server-edit-files-quickly '(");
	}
    else {
      send_string(s,"(server-edit-files '(");
	};

    for (; optind < argc; optind++) {
      if (*argv[optind] == '+')
        starting_line = atoi(argv[optind]);
      else {

        filename_expand(fullpath,argv[optind]);
        sprintf(command,"(%d . \"%s%s\")",starting_line,

#ifdef INTERNET_DOMAIN_SOCKETS
	        remotepath,
#else /* !INTERNET_DOMAIN_SOCKETS */
  	        "",
#endif
	        fullpath);
        send_string(s,command);
        starting_line = 1;
      }; /* else */
    }; /* for */
  }

  send_string(s,"))");

#if defined( WIN32 )
  if (wintofront)
	  showEmacs( uniconify );
#endif

#ifdef SYSV_IPC
  if (connect_type == (int) CONN_IPC)
    disconnect_from_ipc_server(s,msgp,FALSE);
#else /* !SYSV_IPC */
  if (connect_type != (int) CONN_IPC)
    disconnect_from_server(s,FALSE);
#endif /* !SYSV_IPC */

#if defined( WIN32 )
	WSACleanup();
#endif
  exit(0);

} /* main */

#endif /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */
