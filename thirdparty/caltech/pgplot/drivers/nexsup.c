/* nexsup.c--This is a 'support routine' used by the nedriv.f code, */
/* In brief, this is the main interface between the Fortran and */
/* C languages.  It is called from Fortran and uses a UNIX socket */
/* to send messages to the PGPLOT viewer. */

/* 199-Feb-24 - update from nexsup.m - [AFT] */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/time.h>
#include <netdb.h>
#include <stdio.h>

int pgsock=-1;
struct sockaddr_in server;

#ifdef __STDC__
void grgetreply(int ifunc,int *ibuf,int *lbuf)
#else
void grgetreply(ifunc, ibuf, lbuf)
      int   ifunc;
      int   *ibuf;
      int   *lbuf;
#endif
{
/* Used by nexsup to send a message over the socket and wait */
/* for a reply back */
      struct sockaddr_in replyadd;
      struct pgmess {
         unsigned char c1func;
         unsigned char c1len;
         char cmess[256];
      };
      struct pgmess sbuf;
      int    msgsock, repsock;
      int    i, itmp;

/* We need a socket that pgview can reply to.  Create descriptor. */
      repsock = socket(AF_INET, SOCK_STREAM, 0);
      if (repsock < 0) {
         perror("opening stream socket");
         exit(1);
      }

/* Name socket using wildcards */
      replyadd.sin_family = AF_INET;
      replyadd.sin_addr.s_addr = INADDR_ANY;
      replyadd.sin_port = 0;
      if (bind(repsock, (struct sockaddr *)&replyadd, sizeof(replyadd))) {
         perror("binding stream socket");
         exit(1);
      }

/* Find out assigned port number so we can forward to the server. */
      itmp = sizeof(replyadd);
      if (getsockname(repsock, (struct sockaddr *)&replyadd, &itmp)) {
         perror("getting socket name");
         exit(1);
      }

/* Start accepting connections */
      listen(repsock, 5);

/* Now tell pgview the port that we are listen'ing on.  Note, the port */
/* number is already in network byte order which is what pgview needs. */
      sbuf.c1func = ifunc;
      sbuf.c1len = 2;
      memcpy(sbuf.cmess, &replyadd.sin_port, 2);
      if (write(pgsock, &sbuf, sbuf.c1len+2) < 0)
         perror("writing on stream socket");

/* Now wait for the reply */
      msgsock = accept(repsock, (struct sockaddr *)0, (int *)0);
      if (msgsock == -1)
         perror("accept");
      else {
         *lbuf = read(msgsock, ibuf, 16);
      }
      close(msgsock);
      return;
}

#ifdef __STDC__
void nexsup_(int *ifunc, char *cbuf, float rtmp[], int len_cbuf)
#else
void nexsup_(ifunc, cbuf, rtmp, len_cbuf)
      int  *ifunc;
      char *cbuf;
      float rtmp[];
      int len_cbuf;
#endif
{
      struct hostent *hp;
      struct pgmess {
         unsigned char c1func;
         unsigned char c1len;
         char cmess[256];
      };
      struct pgmess sbuf;
      char   *cdis, *cview;
      char   cloc[256];
      int    ibuf[10];
      int    i, icnt, itmp, lbuf, lloc;

      if ( pgsock<0 ) {
         icnt = 0;
         do {
/* Create socket descriptor. */
            pgsock = socket(AF_INET, SOCK_STREAM, 0);
            if ( pgsock < 0) {
               perror("opening stream socket");
            }

/* Create socket address structure */
            server.sin_family = AF_INET;
            cdis = (char *) getenv("DISPLAY");
            if ( cdis==NULL )
               cdis="localhost";
            else {
/* Convert a colon to null (end of string) */
               for (i=0; i<strlen(cdis); i++)  {
                  if ( cdis[i]==':' ) cdis[i]='\0';
               }
            }
            hp = gethostbyname(cdis);
            if (hp == 0) {
               fprintf(stderr, "%s: unknown host", cdis);
            }
            memcpy(&server.sin_addr, hp->h_addr, hp->h_length);
            server.sin_port = htons(7974);

/* Connect descriptor to address */
            itmp=connect(pgsock,(struct sockaddr *)&server,sizeof(server));
            if ( itmp<0 ) {
               close(pgsock);
               if ( icnt==0 ) {
                  cview = (char *) getenv("PGVIEW");
                  if ( cdis==NULL || cview!=NULL ) {
/* If PGVIEW is defined or DISPLAY is not defined, then try to launch pgview */
                     printf("Launching pgview...\n");
                     if ( cview==NULL ) cview="/LocalApps/pgview.app/pgview";
                     strcpy(cloc, cview);
                     lloc=strlen(cloc);
                     cloc[lloc]=' ';
                     cloc[lloc+1]='&';
                     cloc[lloc+2]='\0';
                     system(cloc);
                  } else {
                     printf("Please launch pgview on your display system.\n");
                  }
               }
               sleep(1);
               if((icnt/5)*5 == icnt) printf("waiting...\n");
               icnt=icnt+1;
            }
         } while (itmp<0 && icnt<20);
         if ( itmp < 0 ) {
            printf("Could not find port connected to pgview.\n");
            exit(1);
         }
      }

      switch (*ifunc) {
      case 1:
         grgetreply(1, ibuf, &lbuf);
         rtmp[0]=(float)ntohl(ibuf[0]);
         rtmp[1]=(float)ntohl(ibuf[1]);
         rtmp[2]=(float)ntohl(ibuf[2]);
         rtmp[3]=(float)ntohl(ibuf[3]);
         break;
      case 2:
         sbuf.c1func = 2;
         sbuf.c1len = 0;
         if (write(pgsock, &sbuf, sbuf.c1len+2) < 0)
            perror("writing on stream socket");
         break;
      case 3:
/* Make sure we send the null character at the end. */
         itmp=strlen(cbuf)+1;
         sbuf.c1func = 3;
         sbuf.c1len = itmp;
         memcpy(sbuf.cmess, cbuf, itmp);
         if (write(pgsock, &sbuf, sbuf.c1len+2) < 0)
            perror("writing on stream socket");
         break;
      case 4:
         grgetreply(4, ibuf, &lbuf);
         rtmp[0]=(float)ntohl(ibuf[0]);
         rtmp[1]=(float)ntohl(ibuf[1]);
         rtmp[2]=(float)ntohl(ibuf[2]);
         break;
      case 5:
         sbuf.c1func = 5;
         sbuf.c1len = 0;
         if (write(pgsock, &sbuf, sbuf.c1len+2) < 0)
            perror("writing on stream socket");
         break;
      case 6:
         sbuf.c1func = 6;
         sbuf.c1len = 0;
         if (write(pgsock, &sbuf, sbuf.c1len+2) < 0)
            perror("writing on stream socket");
         break;
      case 7:
         close(pgsock);
         pgsock=-1;
         break;
      default :
         printf("nexsup--Unknown function code= %d\n",*ifunc);
         break;
      }
}
