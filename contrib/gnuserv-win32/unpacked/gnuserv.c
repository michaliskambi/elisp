/* -*-C-*-
 Server code for handling requests from clients and forwarding them
 on to the GNU Emacs process.

 This file is part of GNU Emacs.

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com), based on 'etc/server.c'
         from the 18.52 GNU Emacs distribution.

 Please mail bugs and suggestions to the author at the above address.
*/

/* HISTORY 
 *
 * 7-Sep-1999		guy@wyrdrune.com
 *    Ported internet socket version over to NT.
 *    Part of this is based upon the work that Peter Breton (pbreton@cs.umb.edu) 
 *    and Nico Francois (nico.francois@scala.nl) did on an NT specific version using Mailslots.
 *    Since I wanted to be able to use gnuclient on Unix talking to an Emacs on NT, I needed to get
 *    the internet socket stuff working, so I went back to the original source.
 *
 *    I should probably try to merge the two sources completely so that there is no need to keep two
 *	  incompatible versions hanging around.
 *
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

static char rcsid [] = "$Header: gnuserv.c,v 2.0 94/08/14 12:33:29 arup Exp $";

#include "gnuserv.h"

#ifdef USE_LITOUT
#ifdef linux
#include <bsd/sgtty.h>
#else
#include <sgtty.h>
#endif
#endif

#ifdef AIX
#include <sys/select.h>
#endif

#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && \
    !defined(INTERNET_DOMAIN_SOCKETS)
main ()
{
  fprintf (stderr,"Sorry, the Emacs server is only supported on systems that have\n");
  fprintf (stderr,"Unix Domain sockets, Internet Domain sockets or System V IPC\n");
  exit (1);
} /* main */
#else /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */

#ifdef SYSV_IPC

int ipc_qid = 0;		/* ipc message queue id */
int ipc_wpid = 0;		/* watchdog task pid */


/*
  ipc_exit -- clean up the queue id and queue, then kill the watchdog task
              if it exists. exit with the given status.
*/
void ipc_exit(stat)
     int stat;
{
  msgctl(ipc_qid,IPC_RMID,0);
  
  if (ipc_wpid != 0)
    kill(ipc_wpid,SIGKILL);

  exit(stat);
} /* ipc_exit */


/*
  ipc_handle_signal -- catch the signal given and clean up.
*/
void ipc_handle_signal(sig)
     int sig;
{
  ipc_exit(0);
} /* ipc_handle_signal */


/* 
  ipc_spawn_watchdog -- spawn a watchdog task to clean up the message queue should the
			server process die.
*/
int ipc_spawn_watchdog()
{
  if ((ipc_wpid = fork()) == 0) { /* child process */
    int ppid = getppid();	/* parent's process id */

    setpgrp();			/* gnu kills process group on exit */
    
    while (1) {
      if (kill(ppid,0) < 0) {	/* ppid is no longer valid, parent may have died */
	ipc_exit(0);
      }; /* if */

      sleep(10);		/* have another go later */
    }; /* while */
  }; /* if */

} /* ipc_spawn_watchdog */


/*
  ipc_init -- initialize server, setting the global msqid that can be listened on.
*/
void ipc_init(msgpp)
     struct msgbuf **msgpp;
{
  key_t key;			/* messge key */
  char buf[GSERV_BUFSZ];	/* pathname for key */
  int p;			/* child process id */

  sprintf(buf,"/tmp/gsrv%d",geteuid());
  creat(buf,0600);
  key = ftok(buf,1);

  if ((ipc_qid = msgget(key,0600|IPC_CREAT)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create msg queue\n",progname);
    ipc_exit(1);
  }; /* if */

  ipc_spawn_watchdog();

  signal(SIGTERM,ipc_handle_signal);
  signal(SIGINT,ipc_handle_signal);

  if ((*msgpp = (struct msgbuf *) 
                malloc(sizeof **msgpp + GSERV_BUFSZ)) == NULL) {
    fprintf(stderr,
	    "%s: unable to allocate space for message buffer\n",progname);
    ipc_exit(1);
  }; /* if */

} /* ipc_init */


/*
  handle_ipc_request -- accept a request from a client, pass the request on
  			to the GNU Emacs process, then wait for its reply and
			pass that on to the client.
*/
void handle_ipc_request(msgp)
     struct msgbuf *msgp;	/* message buffer */
{
  struct msqid_ds msg_st;	/* message status */
  char buf[GSERV_BUFSZ];
  int len;			/* length of message / read */
  int s, result_len;            /* tag fields on the response from emacs */
  int junk;			/* junk value */
  int offset = 0;
  int total = 1;                /* # bytes that will actually be sent off */

  if ((len = msgrcv(ipc_qid,msgp,GSERV_BUFSZ-1,1,0)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to receive\n",progname);
    ipc_exit(1);
  }; /* if */

  msgctl(ipc_qid,IPC_STAT,&msg_st);
  strncpy(buf,msgp->mtext,len);
  buf[len] = '\0';		/* terminate */
  
  printf("%d %s",ipc_qid,buf);
  fflush(stdout);

  /* now for the response from gnu */
  msgp->mtext[0] = '\0';

#if 0
  if ((len = read(0,buf,GSERV_BUFSZ)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read\n",progname);
    ipc_exit(1);
  }; /* if */
      
  sscanf(buf,"%d:%[^\n]\n",&junk,msgp->mtext);
#else 

  /* read in "n/m:" (n=client fd, m=message length) */

  while (offset < GSERV_BUFSZ && 
	 ((len = read(0,buf+offset,1)) > 0) &&
	 buf[offset] != ':') {
    offset += len;
  }

  if (len < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read\n",progname);
    exit(1);
  }
      
  /* parse the response from emacs, getting client fd & result length */
  buf[offset] = '\0';
  sscanf(buf,"%d/%d", &s, &result_len);

  while (result_len > 0) {
    if ((len = read(0,buf,min2(result_len, GSERV_BUFSZ - 1))) < 0) {
      perror(progname);
      fprintf(stderr,"%s: unable to read\n",progname);
      exit(1);
    }

    /* Send this string off, but only if we have enough space */ 

    if (GSERV_BUFSZ > total) {
      if (total + len <= GSERV_BUFSZ)
	buf[len] = 0;
      else 
	buf[GSERV_BUFSZ - total] = 0;

      send_string(s,buf);
      total += strlen(buf);
    };

    result_len -= len;
  }

  /* eat the newline */
  while ((len = read(0,buf,1)) == 0)
    ;
  if (len < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read\n",progname);
    exit(1);
  }
  if (buf[0] != '\n') {
    fprintf(stderr,"%s: garbage after result [%c]\n",progname, buf[0]);
    exit(1);
  }
#endif

  /* Send a response back to the client. */

  msgp->mtype = msg_st.msg_lspid;
  if (msgsnd(ipc_qid,msgp,strlen(msgp->mtext)+1,0) < 0)
    perror("msgsend(gnuserv)");

} /* handle_ipc_request */
#endif /* SYSV_IPC */

#if defined( WIN32 )
void win_show_emacs(void)
{
  if (getenv("GNUSERV_SHOW_EMACS")) {
    HWND hWnd = FindWindow ("Emacs", NULL);
    if (hWnd) {
      /* Is the Emacs window iconified ? */
      if (IsIconic (hWnd)) {
        /* ShowWindow (hWnd, SW_SHOWNORMAL ); */  /* for some reason this seems to hang emacs,
												     either way, the following call does the same
													 by getting emacs to uniconify itself */
        /* Need this since Emacs thinks it is still iconified otherwise! */
        SendMessage (hWnd, WM_SYSCOMMAND, SC_RESTORE, 0);
	  }
      else {
        SetForegroundWindow (hWnd);
	  }
	}
  }
}
#endif

#if defined(INTERNET_DOMAIN_SOCKETS) || defined(UNIX_DOMAIN_SOCKETS)
/*
  echo_request -- read request from a given socket descriptor, and send the information
                  to stdout (the gnu process).
*/
void echo_request(s)
int s;				/* socket */
{
  char buf[GSERV_BUFSZ];
  int len;

#if defined( WIN32 )
  win_show_emacs();
#endif

  printf("%d ",s);
  
  /* read until we get a newline or no characters */
  while ((len = recv(s,buf,GSERV_BUFSZ,0)) > 0) {
    buf[len] = '\0';
    printf("%s",buf);

    if (buf[len-1] == EOT_CHR) {
      fflush(stdout);
      break;			/* end of message */
    }

  }; /* while */

  if (len < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to recv\n",progname);
    exit(1);
  }; /* if */
  
} /* echo_request */


#if defined( WIN32 )
void handle_response( int fd )
{
  char buf[GSERV_BUFSZ+1];
  int offset=0;
  int s;
  int soc;
  int len;
  int result_len;

  int addrlen = sizeof(struct sockaddr_in);
  struct sockaddr_in peer;	/* for peer socket address */

  bzero((char *)&peer,sizeof(struct sockaddr_in));

  if ((soc = accept(fd,(struct sockaddr *)&peer,&addrlen)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to accept\n",progname);
    exit(1);
  }; /* if */

  /* read in "n/m:" (n=client fd, m=message length) */
  while (offset < GSERV_BUFSZ && 
	 ((len = recv(soc,buf+offset,1,0)) > 0) &&
	 buf[offset] != ':') {
    offset += len;
  }

  if (len < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read\n",progname);
    exit(1);
  }
      
  /* parse the response from emacs, getting client fd & result length */
  buf[offset] = '\0';
  sscanf(buf,"%d/%d", &s, &result_len);

  /* s == 0 and result_len == -1 if stdin closed on us.  In which cas we exit */

  if ( result_len < 0 )
  {
    fprintf(stderr,"%s: unable to read stdin\n",progname);
    exit(1);
  }

  while (result_len > 0) {
    if ((len = recv(soc,buf,min2(result_len,GSERV_BUFSZ),0)) < 0) {
      perror(progname);
      fprintf(stderr,"%s: unable to read\n",progname);
      exit(1);
    }
    buf[len] = '\0';
    send_string(s,buf);
    result_len -= len;
  }

  closesocket(s);

} /* handle_response */
#else

void handle_response()
{
  char buf[GSERV_BUFSZ+1];
  int offset=0;
  int s;
  int len;
  int result_len;

  /* read in "n/m:" (n=client fd, m=message length) */
  while (offset < GSERV_BUFSZ && 
	 ((len = read(0,buf+offset,1)) > 0) &&
	 buf[offset] != ':') {
    offset += len;
  }

  if (len < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read\n",progname);
    exit(1);
  }
      
  /* parse the response from emacs, getting client fd & result length */
  buf[offset] = '\0';
  sscanf(buf,"%d/%d", &s, &result_len);

  while (result_len > 0) {
    if ((len = read(0,buf,min2(result_len,GSERV_BUFSZ))) < 0) {
      perror(progname);
      fprintf(stderr,"%s: unable to read\n",progname);
      exit(1);
    }
    buf[len] = '\0';
    send_string(s,buf);
    result_len -= len;
  }

  /* eat the newline */
  while ((len = read(0,buf,1)) == 0)
    ;
  if (len < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read\n",progname);
    exit(1);
  }
  if (buf[0] != '\n') {
    fprintf(stderr,"%s: garbage after result\n",progname);
    exit(1);
  }
  close(s);

} /* handle_response */
#endif

#endif /* INTERNET_DOMAIN_SOCKETS || UNIX_DOMAIN_SOCKETS */


#ifdef INTERNET_DOMAIN_SOCKETS
struct entry {
  u_long host_addr;
  struct entry *next;
};

struct entry *permitted_hosts[TABLE_SIZE];


/*
  permitted -- return whether a given host is allowed to connect to the server.
*/
int permitted(host_addr)
     u_long host_addr;
{
  int key;
  struct entry *entry;
  
  /* First find the hash key */
  key = HASH(host_addr) % TABLE_SIZE;
  
  /* Now check the chain for that hash key */
  for(entry=permitted_hosts[key]; entry != NULL; entry=entry->next)
    if (host_addr == entry->host_addr) 
      return(TRUE);

  return(FALSE);

} /* permitted */


/* 
  add_host -- add the given host to the list of permitted hosts, provided it isn't
              already there.
*/	
void add_host(host_addr)
     u_long host_addr;
{
  int key;
  struct entry *new_entry;
  
  if (!permitted(host_addr)) {
    if ((new_entry = (struct entry *) malloc(sizeof(struct entry))) == NULL) {
      fprintf(stderr,"%s: unable to malloc space for permitted host entry\n",
	      progname);
      exit(1);
    }; /* if */

    new_entry->host_addr = host_addr;
    key = HASH(host_addr) % TABLE_SIZE;
    new_entry->next = permitted_hosts[key];
    permitted_hosts[key] = new_entry;
  }; /* if */

} /* add_host */


/*
  setup_table -- initialise the table of hosts allowed to contact the server,
                 by reading from the file specified by the GNU_SECURE
		 environment variable
                 Put in the local machine, and, if a security file is specifed,
                 add each host that is named in the file.
		 Return the number of hosts added.
*/
int setup_table()
{
  FILE *host_file;
  char *file_name;
  char hostname[HOSTNAMSZ];
  u_long host_addr;
  int i, hosts=0;
  
  /* Make sure every entry is null */
  for (i=0; i<TABLE_SIZE; i++)
    permitted_hosts[i] = NULL;

#if _LOCAL_HOST_ACCESS /* Don't even want to allow access from the local host by default */
  gethostname(hostname,HOSTNAMSZ);

  if ((host_addr = internet_addr(hostname)) == -1) {
    fprintf(stderr,"%s: unable to find %s in /etc/hosts or from YP", 
	    progname,hostname);
    exit(1);
  }; /* if */

  add_host(host_addr);					/* add local host */
  hosts++;
#endif 

  if (((file_name = getenv("GNU_SECURE")) != NULL &&    /* security file  */
       (host_file = fopen(file_name,"r")) != NULL)) {	/* opened ok */
    while ((fscanf(host_file,"%s",hostname) != EOF))	/* find a host */
      if ((host_addr = internet_addr(hostname)) != -1) {/* get its addr */
	add_host(host_addr);				/* add the addr */
        hosts++;
      }
    fclose(host_file);
  }; /* if */
  return hosts;
} /* setup_table */


/*
  internet_init -- initialize server, returning an internet socket that can
                    be listened on.
*/
int internet_init()
{
  int ls;			/* socket descriptor */
  struct servent *sp;		/* pointer to service information */
  struct sockaddr_in server;	/* for local socket address */
  char *ptr;			/* ptr to return from getenv */

  if (setup_table() == 0) 
    return -1;

  /* clear out address structure */
  bzero((char *)&server,sizeof(struct sockaddr_in));
  
  /* Set up address structure for the listen socket. */
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;

  /* Find the information for the gnu server
   * in order to get the needed port number.
   */
  if ((ptr=getenv("GNU_PORT")) != NULL)
    server.sin_port = htons((u_short)atoi(ptr));
  else if ((sp = getservbyname ("gnuserv", "tcp")) == NULL)
#if defined( WIN32 )
    server.sin_port = htons(DEFAULT_PORT);
#else
  server.sin_port = htons(DEFAULT_PORT+getuid());
#endif
  else
    server.sin_port = sp->s_port;
  
  /* Create the listen socket. */
  if ((ls = socket (AF_INET,SOCK_STREAM, 0)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */
  
  /* Bind the listen address to the socket. */
  if (bind(ls,(struct sockaddr *) &server,sizeof(struct sockaddr_in)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to bind socket\n",progname);
    exit(1);
  }; /* if */

  /* Initiate the listen on the socket so remote users
   * can connect. 
   */
  if (listen(ls,20) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to listen\n",progname);
    exit(1);
  }; /* if */

  return(ls);

} /* internet_init */

#if defined( WIN32 )

/* rather like connect_to_internet_server() except we make sure that we don't use any environment variables
 * to configure host or port, and make sure that we don't try to run emacs from the server
 */

void win_port_connect(portarg, s)
     int portarg;
     int *s;
{
  char localhost[HOSTNAMSZ];
  struct sockaddr_in peeraddr_in;	/* for peer socket address */

  gethostname(localhost,HOSTNAMSZ);

  bzero((char *)&peeraddr_in,sizeof(struct sockaddr_in));
  peeraddr_in.sin_family = AF_INET;

  /* look up the server host's internet address */
  if ((peeraddr_in.sin_addr.s_addr = internet_addr(localhost)) == -1) {
    fprintf(stderr,"%s: unable to find %s in /etc/hosts or from YP\n",
	    progname,localhost);
    exit(1);
  }
  
  peeraddr_in.sin_port = htons((u_short)portarg);
  
  /* Create the socket. */
  if ((*s = socket (AF_INET,SOCK_STREAM, 0))== -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }
  
  /* Try to connect to the local server at the address
   * which was just built into peeraddr.
   */
  if (connect(*s, (struct sockaddr *)&peeraddr_in,
	      sizeof(struct sockaddr_in)) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to connect to remote\n",progname);
    exit(1);
  }
}

/*
 * This function will run as a separate thread.
 * It will read results from stdin (sent by Emacs) and will send them
 * to the port 
 */

void HandleEmacsResults( void * pPort )
{
  u_short portNo = (u_short)pPort;
  int ls;
#if defined( WIN32 )
  WinsockInit();
#endif

  while (1) {
    // Get handle to stdin
    HANDLE in = GetStdHandle (STD_INPUT_HANDLE);

    // Keep reading (results from Emacs)
    char inputbuff[256];
    DWORD bytesread;
    while ( ReadFile (in, inputbuff, sizeof (inputbuff), &bytesread, (LPOVERLAPPED)NULL)) {
      inputbuff[bytesread] = '\0';
      win_port_connect( portNo, &ls );
      send_string( ls, inputbuff );
	  closesocket(ls);
    }
	if (GetLastError()) {
		// Inform user
		char buff[80];
		sprintf ( buff, "Error %d when reading from stdin!\nAborting.", GetLastError() );
		MessageBox ( NULL, buff, "GnuServ", MB_ICONINFORMATION);

        win_port_connect( portNo, &ls );
        send_string( ls, "0/-1:" );		/* force the stdin reader to exit */
  	    closesocket(ls);

		break;							/* exit the tread */
	}
  }
#if defined( WIN32 )
  WSACleanup();
#endif
}

/*
  WIN32
  win_port_init -- Create an interenet port that we can listen on, a separate thread will
                   monitor stdin (from emacs) and post to this port
*/
int win_port_init()
{
  int ls;			/* socket descriptor */
  struct servent *sp;		/* pointer to service information */
  struct sockaddr_in server;	/* for local socket address */
  char *ptr;			/* ptr to return from getenv */

  /* clear out address structure */
  bzero((char *)&server,sizeof(struct sockaddr_in));
  
  /* Set up address structure for the listen socket. */
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;

  /* Find the information for the gnu server
   * in order to get the needed port number.
   */
  if ((ptr=getenv("GNU_PORT")) != NULL)
    server.sin_port = htons((u_short)(atoi(ptr)+1));
  else if ((sp = getservbyname ("gnuserv", "tcp")) == NULL)
    server.sin_port = htons(DEFAULT_PORT+1);
  else
    server.sin_port = sp->s_port+1;
  
  /* Create the listen socket. */
  if ((ls = socket (AF_INET,SOCK_STREAM, 0)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */
  
  /* Bind the listen address to the socket. */
  if (bind(ls,(struct sockaddr *) &server,sizeof(struct sockaddr_in)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to bind socket\n",progname);
    exit(1);
  }; /* if */

  /* Initiate the listen on the socket so remote users
   * can connect. 
   */
  if (listen(ls,20) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to listen\n",progname);
    exit(1);
  }; /* if */

  {
	/* Create thread to read Emacs results from stdin and send them */
	/* to our mailslot. */
	u_short pNo = ntohs(server.sin_port);
	_beginthread( HandleEmacsResults, 0, (void*)pNo );
  }
  return(ls);

} /* win_port_init */
#endif


/*
  handle_internet_request -- accept a request from a client and send the information
                             to stdout (the gnu process).
*/
void handle_internet_request(ls)
int ls;				/* listen socket */
{
  int s;
  int addrlen = sizeof(struct sockaddr_in);
  struct sockaddr_in peer;	/* for peer socket address */

  bzero((char *)&peer,sizeof(struct sockaddr_in));

  if ((s = accept(ls,(struct sockaddr *)&peer,&addrlen)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to accept\n",progname);
    exit(1);
  }; /* if */
    
  /* Check that access is allowed - if not return crud to the client */
  if (!permitted(peer.sin_addr.s_addr)) {
    send_string(s,"gnudoit: Connection refused\ngnudoit: unable to connect to remote");
#if defined( WIN32 )
	closesocket(s);
#else
    close(s);
#endif
    return;
  }; /* if */

  echo_request(s);
  
} /* handle_internet_request */
#endif /* INTERNET_DOMAIN_SOCKETS */


#ifdef UNIX_DOMAIN_SOCKETS
/*
  unix_init -- initialize server, returning an unix-domain socket that can
               be listened on.
*/
int unix_init()
{
  int ls;			/* socket descriptor */
  struct sockaddr_un server; 	/* unix socket address */

  if ((ls = socket(AF_UNIX,SOCK_STREAM, 0)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */

  /* Set up address structure for the listen socket. */
#ifdef HIDE_UNIX_SOCKET
  sprintf(server.sun_path,"/tmp/gsrvdir%d",geteuid());
  if (mkdir(server.sun_path, 0700) < 0) {
    /* assume it already exists, and try to set perms */
    if (chmod(server.sun_path, 0700) < 0) {
      perror(progname);
      fprintf(stderr,"%s: can't set permissions on %s\n",
	      progname, server.sun_path);
      exit(1);
    }
  }
  strcat(server.sun_path,"/gsrv");
  unlink(server.sun_path);	/* remove old file if it exists */
#else /* HIDE_UNIX_SOCKET */
  sprintf(server.sun_path,"/tmp/gsrv%d",geteuid());
  unlink(server.sun_path);	/* remove old file if it exists */
#endif /* HIDE_UNIX_SOCKET */

  server.sun_family = AF_UNIX;
 
  if (bind(ls,(struct sockaddr *)&server,strlen(server.sun_path)+2) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to bind socket\n",progname);
    exit(1);
  }; /* if */

  chmod(server.sun_path,0700);	/* only this user can send commands */

  if (listen(ls,20) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to listen\n",progname);
    exit(1);
  }; /* if */

  signal(SIGPIPE,SIG_IGN);	/* in case user kills client */

  return(ls);

} /* unix_init */


/*
  handle_unix_request -- accept a request from a client and send the information
                         to stdout (the gnu process).
*/
void handle_unix_request(ls)
int ls;				/* listen socket */
{
  int s;
  int len = sizeof(struct sockaddr_un);
  struct sockaddr_un server; 	/* for unix socket address */

  server.sun_family = AF_UNIX;

  if ((s = accept(ls,(struct sockaddr *)&server,&len)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to accept\n",progname);
  }; /* if */

  echo_request(s);
  
} /* handle_unix_request */
#endif /* UNIX_DOMAIN_SOCKETS */


main(argc,argv)
     int argc;
     char *argv[];
{
  int ils = -1;			/* internet domain listen socket */
  int uls = -1;			/* unix domain listen socket */
  int wls = -1;			/* ip port to tie in with stdin input thread */
  int chan;				/* temporary channel number */
#ifdef SYSV_IPC
  struct msgbuf *msgp;		/* message buffer */
#endif /* SYSV_IPC */

  progname = argv[0];

#if defined( WIN32 )
  WinsockInit();
#endif

  for(chan=3; chan < _NFILE; close(chan++)) /* close unwanted channels */
    ;

#ifdef USE_LITOUT
  {
    /* this is to allow ^D to pass to emacs */
    int d = LLITOUT;
    (void) ioctl(fileno(stdout), TIOCLBIS, &d);
  }
#endif

#ifdef SYSV_IPC
  ipc_init(&msgp);		/* get a msqid to listen on, and a message buffer */
#endif /* SYSV_IPC */

#ifdef INTERNET_DOMAIN_SOCKETS
  ils = internet_init();	/* get a internet domain socket to listen on */
#endif /* INTERNET_DOMAIN_SOCKETS */

#ifdef UNIX_DOMAIN_SOCKETS
  uls = unix_init();		/* get a unix domain socket to listen on */
#endif /* UNIX_DOMAIN_SOCKETS */

#ifdef WIN32
  wls = win_port_init();
#endif

  while (1) {
#ifdef SYSV_IPC
    handle_ipc_request(msgp);
#else /* NOT SYSV_IPC */
    fd_set rmask;
    FD_ZERO(&rmask);
#if ! defined( WIN32 )
    FD_SET(fileno(stdin), &rmask);
#endif
    if (uls >= 0)
      FD_SET(uls, &rmask);
    if (ils >= 0)
      FD_SET(ils, &rmask);
#if defined( WIN32 )
    if (wls >= 0)
      FD_SET(wls, &rmask);
#endif
    
    if (select(max2(wls,max2(fileno(stdin),max2(uls,ils))) + 1, &rmask, 
	       (fd_set *)NULL, (fd_set *)NULL, (struct timeval *)NULL) < 0)
	{
      perror(progname);
      fprintf(stderr,"%s: unable to select\n",progname);
      exit(1);
    }; /* if */

#ifdef UNIX_DOMAIN_SOCKETS
    if (uls > 0 && FD_ISSET(uls, &rmask))
      handle_unix_request(uls);
#endif

#ifdef INTERNET_DOMAIN_SOCKETS
    if (ils > 0 && FD_ISSET(ils, &rmask))
      handle_internet_request(ils);
#endif /* INTERNET_DOMAIN_SOCKETS */

#if defined( WIN32 )
    if (wls > 0 && FD_ISSET(wls, &rmask))
      handle_response(wls);
#else
    if (FD_ISSET(fileno(stdin), &rmask))      /* from stdin (gnu process) */
      handle_response();
#endif
#endif /* NOT SYSV_IPC */
  }; /* while */

#if defined( WIN32 )
	WSACleanup();
#endif
} /* main */

#endif /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */
