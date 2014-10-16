/* -*-C-*-
 Common library code for the GNU Emacs server and client.

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

static char rcsid [] = "$Header: gnuslib.c,v 2.3 94/09/08 17:18:37 arup Exp $";

#include "gnuserv.h"

#if defined( WIN32 )
#include <windows.h>
#endif

/* On some systems, e.g. DGUX, inet_addr returns a 'struct in_addr'. */
#ifdef HAVE_BROKEN_INET_ADDR
# define IN_ADDR struct in_addr
# define NUMERIC_ADDR_ERROR (numeric_addr.s_addr == -1)
#else
# if (LONGBITS > 32)
#  define IN_ADDR unsigned int
# else
#  define IN_ADDR unsigned long
# endif
# define NUMERIC_ADDR_ERROR (numeric_addr == (IN_ADDR) -1)
#endif

char *progname = NULL;

int make_connection(hostarg, portarg, s)
     char *hostarg;
     int portarg;
     int *s;
{
#ifdef INTERNET_DOMAIN_SOCKETS
  char localhost[HOSTNAMSZ];
  char *ptr;
  if (hostarg == NULL)
    hostarg = getenv("GNU_HOST");
  if (portarg == 0 && (ptr=getenv("GNU_PORT")) != NULL)
    portarg = atoi(ptr);
#endif

  if (hostarg != NULL) {
    /* hostname was given explicitly, via cmd line arg or GNU_HOST, 
     * so obey it. */
#ifdef UNIX_DOMAIN_SOCKETS
    if (!strcmp(hostarg, "unix")) {
      *s = connect_to_unix_server();
      return (int) CONN_UNIX;
    } 
      else
#endif /* UNIX_DOMAIN_SOCKETS */
#ifdef INTERNET_DOMAIN_SOCKETS
    {
      *s = connect_to_internet_server(hostarg, portarg);
      return (int) CONN_INTERNET;
    }
#endif
  } else {
    /* no hostname given.  Use unix-domain/sysv-ipc, or
     * internet-domain connection to local host if they're not available. */
#ifdef UNIX_DOMAIN_SOCKETS
    *s = connect_to_unix_server();
    return (int) CONN_UNIX;
#endif
#ifdef SYSV_IPC
    *s = connect_to_ipc_server();
    return (int) CONN_IPC;
#endif
#ifdef INTERNET_DOMAIN_SOCKETS
    gethostname(localhost,HOSTNAMSZ);	  /* use this host by default */    
    *s = connect_to_internet_server(localhost, portarg);
    return (int) CONN_INTERNET;
#endif      
  }
}

#ifdef SYSV_IPC
/*
  connect_to_ipc_server -- establish connection with server process via SYSV IPC
  			   Returns msqid for server if successful.
*/
int connect_to_ipc_server()
{
  int s;			/* connected msqid */
  key_t key;			/* message key */
  char buf[GSERV_BUFSZ+1];		/* buffer for filename */

  sprintf(buf,"/tmp/gsrv%d",geteuid());
  creat(buf,0600);
  if ((key = ftok(buf,1)) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to get ipc key from %s\n",
	    progname, buf);
    exit(1);
  }

  if ((s = msgget(key,0600)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to access msg queue\n",progname);
    exit(1);
  }; /* if */

  return(s);

} /* connect_to_ipc_server */


/*
  disconnect_from_ipc_server -- inform the server that sending has finished,
                                and wait for its reply.
*/
void disconnect_from_ipc_server(s,msgp,echo)
     int s;
     struct msgbuf *msgp;
     int echo;
{
  int len;			/* length of received message */

  send_string(s,EOT_STR);	/* EOT terminates this message */
  msgp->mtype = 1;

  if(msgsnd(s,msgp,strlen(msgp->mtext)+1,0) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to send message to server\n",progname);
    exit(1);
  }; /* if */
  
  if((len = msgrcv(s,msgp,GSERV_BUFSZ,getpid(),0)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to receive message from server\n",progname);
    exit(1);
  }; /* if */

  if (echo) {
    msgp->mtext[len] = '\0';	/* string terminate message */
    fputs(msgp->mtext, stdout);
    if (msgp->mtext[len-1] != '\n') putchar ('\n');
  }; /* if */

} /* disconnect_from_ipc_server */  
#endif /* SYSV_IPC */


#if defined(INTERNET_DOMAIN_SOCKETS) || defined(UNIX_DOMAIN_SOCKETS)
/*
  send_string -- send string to socket.
*/
void send_string(s,msg)
     int s;
     char *msg;
{
#if defined( _WANT_SEND_RECV )
  if (send(s,msg,strlen(msg),0) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to send\n",progname);
    exit(1);
  }; /* if */ 
#else  
  int len, left=strlen(msg);
  while (left > 0) {
    if ((len=write(s,msg,min2(left,GSERV_BUFSZ))) < 0) {
      perror(progname);
      fprintf(stderr,"%s: unable to send\n",progname);
      exit(1);
    }; /* if */
    left -= len;
    msg += len;
  }; /* while */ 
#endif
} /* send_string */
#endif /* INTERNET_DOMAIN_SOCKETS || UNIX_DOMAIN_SOCKETS */


#ifdef UNIX_DOMAIN_SOCKETS
/*
  connect_to_unix_server -- establish connection with server process via a unix-
  			    domain socket. Returns socket descriptor for server
			    if successful.
*/
int connect_to_unix_server()
{
  int s;			/* connected socket descriptor */
  struct sockaddr_un server; 	/* for unix connections */

  if ((s = socket(AF_UNIX,SOCK_STREAM,0)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */
  
  server.sun_family = AF_UNIX;
#ifdef HIDE_UNIX_SOCKET
  sprintf(server.sun_path,"/tmp/gsrvdir%d/gsrv",geteuid());
#else  /* HIDE_UNIX_SOCKET */
  sprintf(server.sun_path,"/tmp/gsrv%d",geteuid());
#endif /* HIDE_UNIX_SOCKET */
  if (connect(s,(struct sockaddr *)&server,strlen(server.sun_path)+2) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to connect to local\n",progname);
    exit(1);
  }; /* if */

  return(s);

} /* connect_to_unix_server */
#endif /* UNIX_DOMAIN_SOCKETS */


#ifdef INTERNET_DOMAIN_SOCKETS
/*
  internet_addr -- return the internet addr of the hostname or
                   internet address passed. Return -1 on error.
*/
int internet_addr(host)
     char *host;
{
  struct hostent *hp;		/* pointer to host info for remote host */
  IN_ADDR numeric_addr;		/* host address */

  numeric_addr = inet_addr(host);
  if (!NUMERIC_ADDR_ERROR)
    return numeric_addr;
  else if ((hp = gethostbyname(host)) != NULL)
    return ((struct in_addr *)(hp->h_addr))->s_addr;
  else
    return -1;

} /* internet_addr */

#if defined( WIN32 )

/* 
   Hunt though the registry for the emacs path, whilst we test for failure, the current
   NT Emacs port relies on this value so it should be a safe bet.
   This avoids needing to put the emacs directory on the PATH
 */

BOOL GetEmacsPath( char * pBuf, int len)
{
  BOOL ret = FALSE;
  HKEY KeyHandle;
  HKEY RegistryHandle;
  LONG ErrorCode;

  if ( ( ErrorCode = RegConnectRegistry( 0, HKEY_LOCAL_MACHINE, &RegistryHandle ) ) == ERROR_SUCCESS ) {
    if ( ( ErrorCode = RegOpenKeyEx( RegistryHandle, "SOFTWARE\\GNU\\Emacs", 0, KEY_READ, &KeyHandle ) ) == ERROR_SUCCESS ) {
      CHAR temp_string[ 2048 ];
      int size_of_buffer = 2048;
      DWORD temp_data_type = (DWORD) REG_SZ;
      memset( temp_string, 0, sizeof( temp_string ) );

      if ( ( ErrorCode = RegQueryValueEx( KeyHandle, "emacs_dir", NULL, &temp_data_type, temp_string, &size_of_buffer ) ) == ERROR_SUCCESS ) {
        if ( size_of_buffer > len )
          pBuf[0] = 0;		/* string too long for buffer */
        else {
          memcpy( pBuf, temp_string, size_of_buffer );
          ret = TRUE;
        }
      }
      RegCloseKey( KeyHandle );
    }
    RegCloseKey( RegistryHandle );
  }
  return ret;
}

BOOL StartEmacs(void)
{
  PROCESS_INFORMATION procinfo;
  STARTUPINFO startupinfo;
  char emacsPath[_MAX_PATH];

  memset (&startupinfo, 0, sizeof (STARTUPINFO));
  startupinfo.cb = sizeof (STARTUPINFO);
  startupinfo.wShowWindow = SW_SHOW;

  if ( GetEmacsPath( emacsPath, sizeof(emacsPath) ) )
  	  strcpy( emacsPath + strlen(emacsPath), "\\bin\\runemacs" );
  else
	  strcpy( emacsPath, "runemacs" );

  if (!CreateProcess (NULL, emacsPath, NULL, NULL, FALSE, CREATE_DEFAULT_ERROR_MODE, NULL, NULL, &startupinfo, &procinfo)) {
    fprintf( stderr, "Could not start runemacs.exe");
    return (FALSE);
  }
  return (TRUE);
}

#endif

/*
  connect_to_internet_server -- establish connection with server process via 
  				an internet domain socket. Returns socket
				descriptor for server if successful.
*/
int connect_to_internet_server(serverhost,port)
     char *serverhost;
     int port;
{
  int s;				/* connected socket descriptor */
  struct servent *sp;			/* pointer to service information */
  struct sockaddr_in peeraddr_in;	/* for peer socket address */

  /* clear out address structures */
  bzero((char *)&peeraddr_in,sizeof(struct sockaddr_in));
  
  /* Set up the peer address to which we will connect. */
  peeraddr_in.sin_family = AF_INET;

  /* look up the server host's internet address */
  if ((peeraddr_in.sin_addr.s_addr = internet_addr(serverhost)) == -1) {
    fprintf(stderr,"%s: unable to find %s in /etc/hosts or from YP\n",
	    progname,serverhost);
    exit(1);
  }; /* if */
  
  if (port == 0) {
    if ((sp = getservbyname ("gnuserv","tcp")) == NULL)
#if ! defined( WIN32 )
		peeraddr_in.sin_port = htons((u_short)(DEFAULT_PORT+getuid()));
#else
		peeraddr_in.sin_port = htons((u_short)(DEFAULT_PORT));
#endif
    else
      peeraddr_in.sin_port = sp->s_port;
  } /* if */
  else
    peeraddr_in.sin_port = htons((u_short)port);
  
  /* Create the socket. */
  if ((s = socket (AF_INET,SOCK_STREAM, 0))== -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */
  
  /* Try to connect to the remote server at the address
   * which was just built into peeraddr.
   */
#if defined( WIN32 )
  {
    int runemacs = 0;
	while (1) {
      if (connect(s, (struct sockaddr *)&peeraddr_in,
	        sizeof(struct sockaddr_in)) == -1) {

		  if (runemacs == 0) {
			// Since we could not connect to the socket we
			// are going to assume this failed since gnuserv was
			// not running.  We'll try to start Emacs by
			// running runemacs.exe and then try again.
			if (!StartEmacs())
			  break;
		  }
		  // Don't wait longer than 30 seconds.  If Emacs hasn't
		  // launched the server by then we give up.
		  // Even succesfully launching Emacs doesn't guarantee it
		  // starting gnuserv, so we have to give up at one time.
		  if (++runemacs < 30) {
			// Try to connect again every second.
			Sleep (1000);
		  }
		  else {
            perror(progname);
            fprintf(stderr, "%s: unable to connect to remote\n",progname);
            exit(1);
		  }
	  }
      else
		break;
	}
  }
#else
  if (connect(s, (struct sockaddr *)&peeraddr_in,
	      sizeof(struct sockaddr_in)) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to connect to remote\n",progname);
    exit(1);
  }; /* if */
#endif

  return(s);

} /* connect_to_internet_server */
#endif /* INTERNET_DOMAIN_SOCKETS */


#if defined(INTERNET_DOMAIN_SOCKETS) || defined(UNIX_DOMAIN_SOCKETS)
/*
  disconnect_from_server -- inform the server that sending has finished, and wait for
                            its reply.
*/
void disconnect_from_server(s,echo)
     int s;
     int echo;
{
#if defined( _WANT_SEND_RECV )
  char buffer[REPLYSIZ+1];
#else
  char buffer[GSERV_BUFSZ+1];
#endif

  int add_newline = 1;
  int length;

  send_string(s,EOT_STR);		/* make sure server gets string */

  if (shutdown(s,1) == -1) {
#ifdef EOPNOTSUPP
    /* Linux (1.0.9) doesn't seem to allow shutdown on a unix-domain socket.
     * It gives EOPNOTSUPP.  Ignore it.   -dkindred@cs.cmu.edu
     */
    if (errno != EOPNOTSUPP)
#endif
      {
        perror(progname);
        fprintf(stderr, "%s: unable to shutdown socket\n",progname);
        exit(1);
      }
  }; /* if */

#if defined( _WANT_SEND_RECV )
  while((length = recv(s,buffer,REPLYSIZ,0)) > 0) {
    buffer[length] = '\0';
    if (echo) fputs(buffer,stdout);
    add_newline = (buffer[length-1] != '\n');
  }; /* while */
#else
  while ((length = read(s,buffer,GSERV_BUFSZ)) > 0) {
    buffer[length] = '\0';
    if (echo) {
      fputs(buffer,stdout);
      add_newline = (buffer[length-1] != '\n');
    }; /* if */
  }; /* while */
#endif
  
  if (echo && add_newline) putchar('\n');

  if(length < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read the reply from the server\n",progname);
    exit(1);
  }; /* if */

} /* disconnect_from_server */  
#endif /* INTERNET_DOMAIN_SOCKETS || UNIX_DOMAIN_SOCKETS */

#if defined( WIN32 )

void WinsockInit()
{
  WORD wVersionRequested;
  WSADATA wsaData;
  int err;
  wVersionRequested = MAKEWORD( 2, 2 );
  err = WSAStartup( wVersionRequested, &wsaData );
  if ( err != 0 ) {
    fprintf(stderr,"%s: unable to load winsock\n",progname);
	exit(1);
	}
}

#endif

