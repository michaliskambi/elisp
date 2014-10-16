CC = cc
DEFINES=
OBJS = gnuclient.o gnudoit.o gnuserv.o gnuslib.o
ALL = gnuclient gnudoit gnuserv
RELEASE= gnuserv-2.0beta

# Add a -I flag pointing make at the directory where it can get your
# emacs' config.h file (usually "src" underneath the top of your emacs tree)
#INCLUDES = -I/afs/cs/misc/lemacs/build/src/lemacs-19.9/src -I.

CFLAGS = -O -g $(DEFINES) $(INCLUDES)
LDFLAGS = -lsocket -lnsl
DEST = /usr/local/lemacs/bin

# why was this the default?
# .DUMMY:		clean

all:		$(ALL)

gnuclient:	gnuclient.o gnuslib.o gnuserv.h 
		$(CC) $(LDFLAGS) -o gnuclient gnuclient.o gnuslib.o

gnudoit:	gnudoit.o gnuslib.o gnuserv.h 
		$(CC) $(LDFLAGS) -o gnudoit gnudoit.o gnuslib.o

gnuserv:	gnuserv.o gnuslib.o gnuserv.h
		$(CC) $(LDFLAGS) -o gnuserv gnuserv.o gnuslib.o

install:        $(ALL)
		mv $(ALL) $(DEST)

clean:
		/bin/rm -f $(ALL) *.o *~

tarchive:	clean
		- mkdir /tmp/$(RELEASE)
		- /bin/rm -f /tmp/$(RELEASE)/*
		cp -p * /tmp/$(RELEASE) 
		/bin/rm /tmp/$(RELEASE)/RCS
		(cd /tmp; tar cf /tmp/$(RELEASE).tar $(RELEASE))
		gzip /tmp/$(RELEASE).tar
		/bin/rm -rf /tmp/$(RELEASE)
		mv -i /tmp/$(RELEASE).tar.gz /usr/anon

# DO NOT DELETE THIS LINE -- make depend depends on it.

gnuclient.o: gnuserv.h  
gnudoit.o: gnuserv.h 
gnuserv.o: gnuserv.h
gnuslib.o: gnuserv.h 
