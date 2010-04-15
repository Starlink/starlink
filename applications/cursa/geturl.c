/*
   Comments converted automatically from C++ to C style
   on Wed May 30 17:23:53 BST 2001.
*/

/*  Name:  */
/*     geturl.h  */
/*  Purpose:  */
/*     Get a url (include file) */
/*  Language:  */
/*     C.  */
/*  Type of Module:  */
/*     Include file.  */
/*  Authors:  */
/*     ACD: A C Davenhall (Edinburgh)  */
/*  History:  */
/*     7/12/00 (ACD): Original version.  */
/*     8/12/00 (ACD): First stable version.  */


/*  Note that most of the following includes and defines are based on  */
/*  the example include files for W.R. Stevens, 'Unix Network Programming'.  */

#include        <sys/types.h>   /* basic system data types  */
#include        <sys/socket.h>  /* basic socket definitions  */
#include        <sys/time.h>    /* timeval{} for select()  */
#include        <time.h>        /* timespec{} for pselect()  */
#include        <netinet/in.h>  /* sockaddr_in{} and other Internet defns  */
#include        <arpa/inet.h>   /* inet(3) functions  */
#include        <errno.h>
#include        <fcntl.h>       /* for nonblocking  */
#include        <netdb.h>
#include        <signal.h>
#include        <stdio.h>
#include        <stdlib.h>
#include        <string.h>
#include        <sys/stat.h>    /* for S_xxx file mode constants  */
#include        <sys/uio.h>     /* for iovec{} and readv/writev  */
#include        <unistd.h>
#include        <sys/wait.h>
#include        <sys/un.h>      /* for Unix domain sockets  */

#define INET_ADDRSTRLEN   16    /* "ddd.ddd.ddd.ddd\0"  */
#define MAXLINE           4096  /* max text line length  */
#define SA                struct sockaddr

typedef int logical;
#define  FALSE  0
#define  TRUE   1

void     ErrorMsg(char *);
int GetHostAndPage (int status,  char *queryUrl,  char *queryHost,
		    int *queryPort, char *queryPage, int bufsize);
int GetHostNumber (int status,  char *queryHost, char *hostNumber);
int RetrieveUrl (int status,  const char *hostNumber,  int queryPort,
		 const char *queryPage, logical echoHttpHead);

/* In general most modern systems will have these functions */
#if !HAVE_INET_NTOP
const char *inet_ntop(int af, const void *src,
                             char *dst, size_t cnt);
#endif
#if !HAVE_INET_PTON
int inet_pton(int af, const char *src, void *dst);
#endif

int main(int argc, char **argv)
{
/*+  */
/*  Name:  */
/*     GETURL  */
/*  Purpose:  */
/*     Retrieve a specified URL and write it to standard output.  */
/*  Language:  */
/*     C.  */
/*  Type of Module:  */
/*     Main program.  */
/*  Description:  */
/*     Retrieve the contents of a specified URL and write them to standard  */
/*     output.  */
/*  Usage:  */
/*     geturl  url-to-be-retrieved  [show-HTTP-header]  */
/*  ADAM Parameters:  */
/*     url-to-be-retrieved  */
/*        The URL whose contents are to be written to standard output.  */
/*     show-HTTP-header  */
/*        If any value is given for this optional argument then the  */
/*        HTTP header at the start of the requested page (which is usually  */
/*        hidden) is echoed to standard output.  */
/*  Examples:  */
/*     geturl  http://www.roe.ac.uk/acdwww/cursa/home.html  */
/*        Retrieve the contents of URL  */
/*        http://www.roe.ac.uk/acdwww/cursa/home.html.  */
/*     geturl  http://www.roe.ac.uk/acdwww/cursa/home.html  head  */
/*        Retrieve the contents of URL  */
/*        http://www.roe.ac.uk/acdwww/cursa/home.html and show the header  */
/*        (which is usually hidden) at the start of the page.  */
/*  Algorithm:  */
/*     If there are either one or two command-line arguments then  */
/*       Set the "echo HTTP header" flag depending on whether there is  */
/*       a second argument or not.  */
/*       Copy the first command-line argument to a local string.  */
/*       Obtain the host and Web page (+query) parts of the URL.  */
/*       If ok then  */
/*         Translate the host name into the corresponding number.  */
/*         Retrieve the URL.  */
/*       end if  */
/*       Report any error.  */
/*     else  */
/*       Report usage instructions.  */
/*     end if  */
/*  Authors:  */
/*     ACD: A C Davenhall (Edinburgh)  */
/*     TIMJ: Tim Jenness (JAC, Hawaii) */
/*  History:  */
/*     5/12/00 (ACD): Original version.  */
/*     2/3/01  (ACD): First stable version.  */
/*     24/4/01 (ACD): Added decoding of the port number from the URL.  */
/*     13 AUG 04 (TIMJ): Only compile in inet_pton and inet_ntop if we need
       to */
/*-  */
/*  Local Variables:  */

#define BUFFER_SIZE 1000

int    status;             /* Running status.  */

char   queryUrl[BUFFER_SIZE];  /* The complete input URL.  */
char   queryHost[BUFFER_SIZE]; /* The host part of the URL.  */
char   queryPage[BUFFER_SIZE]; /* The `Web page on the host' part of the URL.  */
char   hostNumber[INET_ADDRSTRLEN]; /* Numeric representation of the host name.  */
char   errorText[BUFFER_SIZE];    /* Text for error message.  */

int      queryPort;        /* Port number.  */

logical  echoHttpHead;     /* Flag; echo the HTTP header?  */

size_t   maxcp;            /* Max number of chars to copy */

/*.  */


/* Initialise the running status.  */

    status = 0;


/* Check that there is either one or two command-line arguments.  */

    if (argc == 2  ||  argc == 3)
    {


/*    Set the "echo HTTP header" flag depending on whether there is  */
/*    a second argument or not.  */

       if (argc == 3)
       {  echoHttpHead = TRUE;
       }
       else
       {  echoHttpHead = FALSE;
       }


/*    Copy the first command-line argument to a local string.  */

       strncpy(queryUrl, argv[1],BUFFER_SIZE - 1);
       queryUrl[BUFFER_SIZE-1] = '\0';

/*    Obtain the host and Web page (+query) parts of the URL and proceed  */
/*    if all is ok.  */

       status = GetHostAndPage(status, queryUrl, queryHost, &queryPort,
         queryPage, BUFFER_SIZE);

       if (status == 0)
       {


/*       Translate the host name into the corresponding number.  */

          status = GetHostNumber(status, queryHost, hostNumber);

/*       Retrieve the URL.  */

          status = RetrieveUrl(status, hostNumber, queryPort, queryPage,
            echoHttpHead);
       }


/*    Report any error.  */

       if (status != 0)
       {
          strcpy(errorText, "unable to retrieve URL ");
          maxcp = BUFFER_SIZE - 1 - strlen( errorText );
          strncat(errorText, queryUrl, maxcp);
	  errorText[BUFFER_SIZE-1] = '\0';
          ErrorMsg(errorText);
	  return EXIT_FAILURE;
       }
    }
    else
    {  printf("! Usage:-\n");
       printf("!   geturl  URL-to-be-retrieved\n");
    }
    return EXIT_SUCCESS;
}

/* ----------------------------------------------------------   */

void ErrorMsg (char *errorBuff)
{
/*+  */
/*  Name:  */
/*     ErrorMsg  */
/*  Purpose:  */
/*     Report an error message to standard output.  */
/*  Language:  */
/*     C.  */
/*  Description:  */
/*     Report an error message to standard output.  */
/*  Arguments:  */
/*     char *errorBuff  (RETURNED)  */
/*        Pointer to a string comprising the message to be reported.  */
/*  Algorithm:  */
/*     Report the message.  */
/*  Authors:  */
/*     ACD: A C Davenhall (Edinburgh)  */
/*  History:  */
/*     5/12/00 (ACD): Original version.  */
/*  Bugs:  */
/*     None known  */
/*-  */
/*  Local Variables:  */

/*.  */

   printf("! Failure: %s\n", errorBuff);

}

/* ----------------------------------------------------------   */


void ErrorReport (char *errorBuff)
{
/*+  */
/*  Name:  */
/*     ErrorReport  */
/*  Purpose:  */
/*     Report a socket/network error message to standard output.  */
/*  Language:  */
/*     C.  */
/*  Description:  */
/*     Report a socket/network error message to standard output.  */
/*  Arguments:  */
/*     char *errorBuff  (RETURNED)  */
/*        Pointer to a string comprising the message to be reported.  */
/*  Algorithm:  */
/*     Report the message.  */
/*  Authors:  */
/*     ACD: A C Davenhall (Edinburgh)  */
/*  History:  */
/*     5/12/00 (ACD): Original version.  */
/*     8/23/00 (ACD): First stable version.  */
/*  Bugs:  */
/*     None known  */
/*-  */
/*  Local Variables:  */

char errnoText[80];    /* Text for translated errno code.  */

/*.  */

   ErrorMsg(errorBuff);

   sprintf(errnoText, "%s (error code %d).", strerror(errno), errno);
   ErrorMsg(errnoText);
}

/* ----------------------------------------------------------   */


int GetHostAndPage (int status,  char *queryUrl,  char *queryHost,
int *queryPort, char *queryPage, int bufsize)
{
/*+  */
/*  Name:  */
/*     GetHostAndPage  */
/*  Purpose:  */
/*     Split A URL into the host name and Web page.  */
/*  Language:  */
/*     C.  */
/*  Description:  */
/*     Split A URL into the host name and Web page (+ query).  */
/*  Arguments:  */
/*     int status  (GIVEN)  */
/*        Input status.  */
/*     char *queryUrl  (GIVEN)  */
/*        Pointer to the input URL.  */
/*     char *queryHost  (RETURNED)  */
/*        Pointer to the Host name.  */
/*     int  *queryPort  (RETURNED)  */
/*        Pointer to the port number.  */
/*     char *queryPage  (RETURNED)  */
/*        Pointer to the Web page (+ query).  */
/*     int bufsize (GIVEN) */
/*        Size of string arrays to be filled */
/*  Algorithm:  */
/*     If the first 7 characters are 'http://' then  */
/*       Extract the URL beyond the 'http://'.  */
/*       Attempt to find the first '/' in the URL.  */
/*       If ok then  */
/*         Extract the host name (+ port).  */
/*         Extract the Web page (+ query).  */
/*       else  */
/*         Copy the host name (+ port).  */
/*         Set the Web page to blank.  */
/*       end if  */
/*       Attempt to find the first ':' in the URL.  */
/*       If ok then  */
/*         Extract the host name.  */
/*         Extract the port number.  */
/*       else  */
/*         Copy the host name.  */
/*         Set the port number to 80.  */
/*     else  */
/*       Set the status.  */
/*     end if  */
/*     If the status is not ok then  */
/*       Report an error.  */
/*     end if  */
/*  Authors:  */
/*     ACD: A C Davenhall (Edinburgh)  */
/*  History:  */
/*     5/12/00 (ACD): Original version.  */
/*     8/12/00 (ACD): First stable version.  */
/*     23/4/01 (ACD): Added decoding the port number and support for  */
/*        URLs with no query and trailing '/'.  */
/*  Bugs:  */
/*     None known  */
/*-  */
/*  Local Variables:  */

char   *httpstr;     /* Pointer to the string 'http://'.  */
char   *query;       /* Pointer to the URL (without the leading 'http://').  */
char   *strokestr;   /* Pointer to the first '/' in the URL.  */
char   *colonstr;    /* Pointer to the first ':' in the URL.  */

char   hostAndPort[BUFFER_SIZE]; /* Buffer for the host and port string.  */
char   charPort[10];     /* Buffer for the port number as a string.  */
char   errorBuff[BUFFER_SIZE];    /* Buffer for error message.  */

size_t hostLen;      /* Length of the host name.  */
size_t maxcp;        /* Max number of chars to copy */

int port;            /* Port number.  */
/*.  */

   if (status == 0)
   {


/*   Check whether the first 7 characters are 'http://'.  */

      httpstr = strstr(queryUrl, "http://");

      if (httpstr != NULL)
      {


/*      Extract the URL beyond the 'http://' (increment the pointer to  */
/*      the start of the string by 7!  */

         query = httpstr + 7;


/*      Attempt to find the first '/' in the URL (this '/' separates the  */
/*      the host name from the Web page (+ query).  Then extract any  */
/*      query which is beyond this character.  */

         strokestr = strstr(query, "/");

         if (strokestr != NULL)
         {


/*         Extract the host name.  */

            hostLen = strokestr - query;

	    if (hostLen > bufsize ) {
	      ErrorMsg( "Length of URL exceeded buffer size\n" );
	      return 1;
	    }

            strncpy(hostAndPort, query, hostLen);
            hostAndPort[hostLen] = '\0';


/*         Extract the Web page (+ query).  */

            strncpy(queryPage, strokestr, bufsize-1);
	    queryPage[bufsize-1] = '\0';
         }
         else
         {  strncpy(hostAndPort, query, BUFFER_SIZE - 1);
            hostAndPort[BUFFER_SIZE -1 ] = '\0';
            strcpy(queryPage, "/");
         }


/*      Attempt to find the first ':' in the URL (this ':' separates the  */
/*      the host name proper the port number.  Then extract the host name  */
/*      proper and any port number.  */

         colonstr = strstr(hostAndPort, ":");

         if (colonstr != NULL)
         {


/*         Extract the host name.  */

            hostLen = colonstr - hostAndPort;

	    if (hostLen > bufsize ) {
	      ErrorMsg( "Length of URL exceeded buffer size\n" );
	      return 1;
	    }

            strncpy(queryHost, hostAndPort, hostLen);
            queryHost[hostLen] = '\0';


/*         Extract the Web page (+ query).  */

            colonstr = colonstr + 1;

            strncpy(charPort, colonstr, bufsize-1);
	    charPort[bufsize-1] = '\0';

            sscanf(charPort, "%d", &port);
         }
         else
         {
            strncpy(queryHost, hostAndPort, bufsize-1);
	    queryHost[bufsize-1] = '\0';
            port = 80;
         }

         *queryPort = port;
      }
      else
      {  status = 1;
      }


/*   Report any error.  */

      if (status != 0)
      {  strcpy(errorBuff, "bad URL: ");
         maxcp = BUFFER_SIZE - 1 - strlen( errorBuff );
         strncat(errorBuff, queryUrl, maxcp);
         errorBuff[BUFFER_SIZE-1] = '\0';
         ErrorMsg(errorBuff);
      }
  }

/*Set the return status.  */

   return status;
}

/* ----------------------------------------------------------   */


int GetHostNumber (int status,  char *queryHost, char *hostNumber)
{
/*+  */
/*  Name:  */
/*     GetHostNumber  */
/*  Purpose:  */
/*     Translate the host name into the corresponding number.  */
/*  Language:  */
/*     C.  */
/*  Description:  */
/*     Translate the host name into the corresponding number.  */
/*  Arguments:  */
/*     int status  (GIVEN)  */
/*        Input status.  */
/*     char *queryHost  (GIVEN)  */
/*        Pointer to the Host name.  */
/*     char *hostNumber  (RETURNED)  */
/*        Pointer to host number.  */
/*  Algorithm:  */
/*     Look up the hexadecimal host number.  */
/*     If ok then  */
/*       Convert the hexadecimal number to the decimal 'dot'  */
/*       representation.  */
/*     end if  */
/*     If not ok then  */
/*       Report error.  */
/*     end if  */
/*  Authors:  */
/*     ACD: A C Davenhall (Edinburgh)  */
/*  History:  */
/*     5/12/00 (ACD): Original version.  */
/*     6/3/01  (ACD): First stable version.  */
/*  Bugs:  */
/*     None known  */
/*-  */
/*  Local Variables:  */
struct hostent  *hptr;            /* Pointer to host record.  */
char            **addlptr;        /* Pointer to top of address list.  */

char   numBuff[INET_ADDRSTRLEN];  /* Buffer host number.  */

char   errorBuff[80];             /* Buffer for error message.  */

/*.  */

   if (status == 0)
   {
     /* if the host name is greater than 100 characters we probably
	have a problem. Need to do this since on linux gethostbyname
	core dumps if we pass in a large string. Do not know what the
	maximum length should be.
     */
     if (strlen(queryHost) > 100 ) {
       status = 1;
       strcpy(errorBuff, "host name seems to be unreasonably large: ");
       strncat(errorBuff, queryHost, 80 - 1 - strlen(errorBuff));
       errorBuff[80] = '\0';
       ErrorMsg(errorBuff);
       return status;
     }

      hptr = gethostbyname(queryHost);
      if (errno == 0)
      {  addlptr = hptr->h_addr_list;
         inet_ntop(AF_INET, *addlptr, numBuff, sizeof(numBuff));
         strncpy(hostNumber, numBuff, sizeof(numBuff) );
         hostNumber[strlen(numBuff)] = '\0';
      }

      if (errno != 0)
      {  status = 1;

         strcpy(errorBuff, "unable to translate host name ");
         strncat(errorBuff, queryHost, 80 - 1 - strlen(errorBuff));
	 errorBuff[80] = '\0';
         ErrorReport(errorBuff);
      }
   }


/*Set the return status.  */

   return status;
}

/* ----------------------------------------------------------   */


int ReadUrl (int status,  int sockfd,  logical echoHttpHead)
{
/*+  */
/*  Name:  */
/*      ReadUrl  */
/*  Purpose:  */
/*     Read a Web page from a socket and write them to standard output.  */
/*  Language:  */
/*     C.  */
/*  Description:  */
/*     Read the contents of a Web page from a socket and write them to  */
/*     standard output.  */
/*  Arguments:  */
/*     int status  (GIVEN)  */
/*        Input status.  */
/*     int sockfd  (GIVEN)  */
/*        Socket descriptor.  */
/*     logical echoHttpHead  (GIVEN)  */
/*        Flag indicating whether the HTTP header at the start of the  */
/*        the page is to be echoed to standard output or not.  */
/*  Algorithm:  */
/*     Do while (there is more input from the socket)  */
/*       Attempt to read some characters from the socket.  */
/*       If any were read then  */
/*         If not removing the HTTP header then  */
/*           Print the characters read.  */
/*         else  */
/*           If the HTTP header has already been removed then  */
/*             Print the characters read.  */
/*           else  */
/*             Attempt to find the end of the HTTP header.  */
/*             If the header was found then  */
/*               Skip the characters before the header.  */
/*               Print the characters after the end of the header.  */
/*             end if  */
/*           end if  */
/*         end if  */
/*       else  */
/*         Set the termination flag.  */
/*       end if  */
/*     end do  */
/*     Report any error.  */
/*  Authors:  */
/*     ACD: A C Davenhall (Edinburgh)  */
/*  History:  */
/*     7/12/00 (ACD): Original version.  */
/*     5/4/01  (ACD): First stable version.  */
/*  Bugs:  */
/*     None known  */
/*-  */
/*  Local Variables:  */
char    newBuff[MAXLINE+1]; /* New characters read.  */
char   *eohptr;             /* Pointer to the end of the HTTP header.  */

logical moreInput;          /* Flag; more input expected?  */
logical headerRemoved;      /* Flag; has the HTTP header been removed?  */

int     numChar;            /* Number of characters read.  */

int     loop;       /* Loop index.  */
int     start;      /* First character to be listed (after skipped header).  */
/*.  */

   if (status == 0)
   {


/*   Intialise the counters, flags etc.  */

      headerRemoved = FALSE;
      moreInput = TRUE;

      while(moreInput == TRUE)
      {


/*      Attempt to read some characters from the socket and proceed if  */
/*      any were read.  */

         numChar = read(sockfd, newBuff, sizeof(newBuff));
         if (numChar != 0)
         {


/*         Check whether the HTTP header is to be removed.  If not, then  */
/*         simply print the characters received.  */

            if (echoHttpHead == TRUE)
            {  for (loop = 0; loop < numChar; loop++)
               {  fputc(newBuff[loop], stdout);
               }
            }
            else
            {


/*            The HTTP header is to be removed.  First check whether it  */
/*            has already gone.  If so then simply print the current  */
/*            buffer of characters.  */

               if (headerRemoved == TRUE)
               {  for (loop = 0; loop < numChar; loop++)
                  {  fputc(newBuff[loop], stdout);
                  }
               }
               else
               {


/*               Check for the end of the HTTP header in the accummulating  */
/*               buffer containing the Web page.  The end of the header is  */
/*               indicated by a line containing just "\r\n".  (Every HTTP  */
/*               line ends in "\r\n".  Therefore for an empty line the  */
/*               sequence "\r\n\r\n" should occur.)  */

                  eohptr = strstr(newBuff, "\r\n\r\n");
                  if (eohptr != NULL)
                  {


/*                  The end of the HTTP buffer was found.  Skip over it  */
/*                  and print the remainder of the Web page which has  */
/*                  accummulated so far.  Then set the flag saying that  */
/*                  the header has been removed.  */

                     start = 4 + (int)(eohptr - newBuff);
                     for (loop = start; loop < numChar; loop++)
                     {  fputc(newBuff[loop], stdout);
                     }

                     headerRemoved = TRUE;
                  }
               }
            }
         }
         else
         {


/*         No more characters were read from the input socket; set the  */
/*         termination flag.  */

            moreInput = FALSE;
         }
      }


/*   Report any error.  */

      if (errno != 0)
      {  status = 1;
         ErrorReport("unable to read the Web page.");
      }
   }


/*Set the return status.  */

   return status;
}

/* ----------------------------------------------------------   */


int RetrieveUrl (int status,  const char *hostNumber,  int queryPort,
  const char *queryPage, logical echoHttpHead)
{
/*+  */
/*  Name:  */
/*     RetrieveUrl  */
/*  Purpose:  */
/*     Retrieve the contents of a URL and write them to standard output.  */
/*  Language:  */
/*     C.  */
/*  Description:  */
/*     Retrieve the contents of a URL and write them to standard output.  */
/*     The URL has previously been decomposed into a host number (in  */
/*     the 'dot notation' and a Web page.  The Web page may optionally  */
/*     have query parameters appended.  */
/*  Arguments:  */
/*     int status  (GIVEN)  */
/*        Input status.  */
/*     char *hostNumber  (GIVEN)  */
/*        Pointer to host number.  */
/*     int queryPort  (GIVEN)  */
/*        Port number for host.  */
/*     char *queryPage  (GIVEN)  */
/*        Pointer to the Web page.  */
/*     logical echoHttpHead  (GIVEN)  */
/*        Flag indicating whether the HTTP header at the start of the  */
/*        the page is to be echoed to standard output or not.  */
/*  Algorithm:  */
/*     Attempt to obtain a socket for accessing the URL.  */
/*     If ok then  */
/*       Assemble the structure containing the server details.  */
/*       Attempt to convert the server address from 'dot notation' to  */
/*       hexadecimal.  */
/*       If ok then  */
/*         Attempt to connect to the socket.  */
/*         If ok then  */
/*           Assemble the HTTP request to GET the contents of the   */
/*           Web page.  */
/*           Attempt to write the HTTP GET to the socket.  */
/*           If ok then  */
/*             Read the contents of the page from the socket and write  */
/*             them to standard output.  */
/*           else  */
/*             Report error: failed to write to socket.  */
/*           end if  */
/*         else  */
/*           Report error: failed to connect to socket.  */
/*         end if  */
/*       else  */
/*         Report error: failed to convert host address.  */
/*       end if  */
/*     else  */
/*       Report error: failed to obtain to socket.  */
/*     end if  */
/*  Authors:  */
/*     ACD: A C Davenhall (Edinburgh)  */
/*  History:  */
/*     6/12/00 (ACD): Original version.  */
/*     8/12/00 (ACD): First stable version.  */
/*     24/4/01 (ACD): Made the port number a passed argument rather than  */
/*        hard-wired as 80.  */
/*  Bugs:  */
/*     None known  */
/*-  */
/*  Local Variables:  */
int    sockfd;                    /* Socket descriptor.  */

struct sockaddr_in   servaddr;    /* Server details structure.  */

char   httpBuffer[BUFFER_SIZE];   /* Buffer for HTTP GET string.  */
char   errorBuff[80];             /* Buffer for error message.  */
/*.  */

   if (status == 0)
   {


/*   Attempt to obtain a socket for accessing the URL and proceed if ok.  */

      sockfd = socket(AF_INET, SOCK_STREAM, 0);
      if (errno == 0)
      {


/*      Assemble the structure containing the server details.  */

#if HAVE_MEMSET
 	 memset(&servaddr, 0, sizeof(servaddr));
#elif HAVE_BZERO
         bzero(&servaddr, sizeof(servaddr));
#else
#  error "Do not know how to set memory to fixed value"
#endif

         servaddr.sin_family = AF_INET;
         servaddr.sin_port   = htons(queryPort);


/*      Attempt to convert the server address from 'dot notation' to  */
/*      hexadecimal and proceed if ok.  */

         inet_pton(AF_INET, hostNumber, &servaddr.sin_addr);
         if (errno == 0)
         {


/*         Attempt to connect to the socket and proceed if ok.  */

            connect(sockfd, (SA *) &servaddr, sizeof(servaddr));
            if (errno == 0)
            {


/*            Assemble the HTTP request to GET the contents of the   */
/*            Web page.  */

	      /* Abort if we do not think we can do this without a
		 buffer overflow. Be safe rather than accurate
		 since being accurate will be ugly for now.
	      */
	      /* Assume rest of header is < 100 characters */
	      if ((strlen(queryPage)+100) > BUFFER_SIZE -1 ) {
		 status = 1;
		 ErrorMsg("Buffer overflow forming http request!\n");
		 return status;
	      }

               strcpy(httpBuffer, "GET ");
               strcat(httpBuffer, queryPage);
               strcat(httpBuffer, " HTTP/1.0\n");

               strcat(httpBuffer, "Accept: text/plain\n");
               strcat(httpBuffer, "Accept: text/html\n");
               strcat(httpBuffer, "User-Agent: geturl\n\n");

/*               printf("httpBuffer: %s", httpBuffer);  */


/*            Attempt to write the HTTP GET to the socket and proceed if  */
/*            ok.  */

               write(sockfd, httpBuffer, strlen(httpBuffer));
               if (errno == 0)
               {


/*               Read the contents of the page from the socket and write  */
/*               them to standard output.  */

                  status = ReadUrl(status, sockfd, echoHttpHead);
               }
               else
               {  status = 1;
                  ErrorReport("unable to write to socket.");
               }
            }
            else
            {  status = 1;
               ErrorReport("unable to connect to socket.");
            }
         }
         else
         {  status = 1;

            strcpy(errorBuff, "unable to convert host address ");
            strcat(errorBuff, hostNumber);

            ErrorReport(errorBuff);
         }
      }
      else
      {  status = 1;
         ErrorReport("unable to obtain a socket.");
      }
   }


/*Set the return status.  */

   return status;
}

/* Only compile this if we need it */
#if !HAVE_INET_NTOP

/* ----------------------------------------------------------   */
/* This is from the BIND 4.9.4 release, modified to compile by itself */

/* Copyright (c) 1996 by Internet Software Consortium.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE CONSORTIUM DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET SOFTWARE
 * CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char rcsid[] = "$Id$";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

#define	IN6ADDRSZ	16
#define	INT16SZ		 2

#ifndef	AF_INET6
#define	AF_INET6	AF_MAX+1	/* just to let this compile */
#endif

/*
 * WARNING: Don't even consider trying to compile this on a system where
 * sizeof(int) < 4.  sizeof(int) > 4 is fine; all the world's not a VAX.
 */

static const char *inet_ntop4(const u_char *src, char *dst, socklen_t size);
static const char *inet_ntop6(const u_char *src, char *dst, socklen_t size);

/* char *
 * inet_ntop(af, src, dst, size)
 *	convert a network format address to presentation format.
 * return:
 *	pointer to presentation format address (`dst'), or NULL (see errno).
 * author:
 *	Paul Vixie, 1996.
 */
const char *
inet_ntop(af, src, dst, size)
	int af;
	const void *src;
	char *dst;
	socklen_t size;
{
	switch (af) {
	case AF_INET:
		return (inet_ntop4(src, dst, size));
	case AF_INET6:
		return (inet_ntop6(src, dst, size));
	default:
		errno = EAFNOSUPPORT;
		return (NULL);
	}
	/* NOTREACHED */
}

/* const char *
 * inet_ntop4(src, dst, size)
 *	format an IPv4 address, more or less like inet_ntoa()
 * return:
 *	`dst' (as a const)
 * notes:
 *	(1) uses no statics
 *	(2) takes a u_char* not an in_addr as input
 * author:
 *	Paul Vixie, 1996.
 */
static const char *
inet_ntop4(src, dst, size)
	const u_char *src;
	char *dst;
	socklen_t size;
{
	static const char fmt[] = "%u.%u.%u.%u";
	char tmp[sizeof "255.255.255.255"];

	sprintf(tmp, fmt, src[0], src[1], src[2], src[3]);
	if (strlen(tmp) > size) {
		errno = ENOSPC;
		return (NULL);
	}
	strcpy(dst, tmp);
	return (dst);
}

/* const char *
 * inet_ntop6(src, dst, size)
 *	convert IPv6 binary address into presentation (printable) format
 * author:
 *	Paul Vixie, 1996.
 */
static const char *
inet_ntop6(src, dst, size)
	const u_char *src;
	char *dst;
	socklen_t size;
{
	/*
	 * Note that int32_t and int16_t need only be "at least" large enough
	 * to contain a value of the specified size.  On some systems, like
	 * Crays, there is no such thing as an integer variable with 16 bits.
	 * Keep this in mind if you think this function should have been coded
	 * to use pointer overlays.  All the world's not a VAX.
	 */
	char tmp[sizeof "ffff:ffff:ffff:ffff:ffff:ffff:255.255.255.255"], *tp;
	struct { int base, len; } best, cur;
	u_int words[IN6ADDRSZ / INT16SZ];
	int i;

	/*
	 * Preprocess:
	 *	Copy the input (bytewise) array into a wordwise array.
	 *	Find the longest run of 0x00's in src[] for :: shorthanding.
	 */
	memset(words, 0, sizeof words);
	for (i = 0; i < IN6ADDRSZ; i++)
		words[i / 2] |= (src[i] << ((1 - (i % 2)) << 3));
	best.base = -1;
	cur.base = -1;
	for (i = 0; i < (IN6ADDRSZ / INT16SZ); i++) {
		if (words[i] == 0) {
			if (cur.base == -1)
				cur.base = i, cur.len = 1;
			else
				cur.len++;
		} else {
			if (cur.base != -1) {
				if (best.base == -1 || cur.len > best.len)
					best = cur;
				cur.base = -1;
			}
		}
	}
	if (cur.base != -1) {
		if (best.base == -1 || cur.len > best.len)
			best = cur;
	}
	if (best.base != -1 && best.len < 2)
		best.base = -1;

	/*
	 * Format the result.
	 */
	tp = tmp;
	for (i = 0; i < (IN6ADDRSZ / INT16SZ); i++) {
		/* Are we inside the best run of 0x00's? */
		if (best.base != -1 && i >= best.base &&
		    i < (best.base + best.len)) {
			if (i == best.base)
				*tp++ = ':';
			continue;
		}
		/* Are we following an initial run of 0x00s or any real hex? */
		if (i != 0)
			*tp++ = ':';
		/* Is this address an encapsulated IPv4? */
		if (i == 6 && best.base == 0 &&
		    (best.len == 6 || (best.len == 5 && words[5] == 0xffff))) {
			if (!inet_ntop4(src+12, tp, sizeof tmp - (tp - tmp)))
				return (NULL);
			tp += strlen(tp);
			break;
		}
		sprintf(tp, "%x", words[i]);
		tp += strlen(tp);
	}
	/* Was it a trailing run of 0x00's? */
	if (best.base != -1 && (best.base + best.len) == (IN6ADDRSZ / INT16SZ))
		*tp++ = ':';
	*tp++ = '\0';

	/*
	 * Check for overflow, copy, and we're done.
	 */
	if ((tp - tmp) > size) {
		errno = ENOSPC;
		return (NULL);
	}
	strcpy(dst, tmp);
	return (dst);
}

/* !HAVE_INET_NTOP */
#endif

/* Only use this if we need to */
#if !HAVE_INET_PTON

/* ----------------------------------------------------------   */
/* This is from the BIND 4.9.4 release, modified to compile by itself */

/* Copyright (c) 1996 by Internet Software Consortium.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND INTERNET SOFTWARE CONSORTIUM DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL INTERNET SOFTWARE
 * CONSORTIUM BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char rcsid[] = "$Id$";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <errno.h>

#define	IN6ADDRSZ	16
#define	INADDRSZ	 4
#define	INT16SZ		 2

#ifndef	AF_INET6
#define	AF_INET6	AF_MAX+1	/* just to let this compile */
#endif

/*
 * WARNING: Don't even consider trying to compile this on a system where
 * sizeof(int) < 4.  sizeof(int) > 4 is fine; all the world's not a VAX.
 */

static int	inet_pton4(const char *src, u_char *dst);
static int	inet_pton6(const char *src, u_char *dst);

/* int
 * inet_pton(af, src, dst)
 *	convert from presentation format (which usually means ASCII printable)
 *	to network format (which is usually some kind of binary format).
 * return:
 *	1 if the address was valid for the specified address family
 *	0 if the address wasn't valid (`dst' is untouched in this case)
 *	-1 if some other error occurred (`dst' is untouched in this case, too)
 * author:
 *	Paul Vixie, 1996.
 */
int
inet_pton(af, src, dst)
	int af;
	const char *src;
	void *dst;
{
	switch (af) {
	case AF_INET:
		return (inet_pton4(src, dst));
	case AF_INET6:
		return (inet_pton6(src, dst));
	default:
		errno = EAFNOSUPPORT;
		return (-1);
	}
	/* NOTREACHED */
}

/* int
 * inet_pton4(src, dst)
 *	like inet_aton() but without all the hexadecimal and shorthand.
 * return:
 *	1 if `src' is a valid dotted quad, else 0.
 * notice:
 *	does not touch `dst' unless it's returning 1.
 * author:
 *	Paul Vixie, 1996.
 */
static int
inet_pton4(src, dst)
	const char *src;
	u_char *dst;
{
	static const char digits[] = "0123456789";
	int saw_digit, octets, ch;
	u_char tmp[INADDRSZ], *tp;

	saw_digit = 0;
	octets = 0;
	*(tp = tmp) = 0;
	while ((ch = *src++) != '\0') {
		const char *pch;

		if ((pch = strchr(digits, ch)) != NULL) {
			u_int new = *tp * 10 + (pch - digits);

			if (new > 255)
				return (0);
			*tp = new;
			if (! saw_digit) {
				if (++octets > 4)
					return (0);
				saw_digit = 1;
			}
		} else if (ch == '.' && saw_digit) {
			if (octets == 4)
				return (0);
			*++tp = 0;
			saw_digit = 0;
		} else
			return (0);
	}
	if (octets < 4)
		return (0);
	/* bcopy(tmp, dst, INADDRSZ); */
	memcpy(dst, tmp, INADDRSZ);
	return (1);
}

/* int
 * inet_pton6(src, dst)
 *	convert presentation level address to network order binary form.
 * return:
 *	1 if `src' is a valid [RFC1884 2.2] address, else 0.
 * notice:
 *	(1) does not touch `dst' unless it's returning 1.
 *	(2) :: in a full address is silently ignored.
 * credit:
 *	inspired by Mark Andrews.
 * author:
 *	Paul Vixie, 1996.
 */
static int
inet_pton6(src, dst)
	const char *src;
	u_char *dst;
{
	static const char xdigits_l[] = "0123456789abcdef",
			  xdigits_u[] = "0123456789ABCDEF";
	u_char tmp[IN6ADDRSZ], *tp, *endp, *colonp;
	const char *xdigits, *curtok;
	int ch, saw_xdigit;
	u_int val;

	memset((tp = tmp), 0, IN6ADDRSZ);
	endp = tp + IN6ADDRSZ;
	colonp = NULL;
	/* Leading :: requires some special handling. */
	if (*src == ':')
		if (*++src != ':')
			return (0);
	curtok = src;
	saw_xdigit = 0;
	val = 0;
	while ((ch = *src++) != '\0') {
		const char *pch;

		if ((pch = strchr((xdigits = xdigits_l), ch)) == NULL)
			pch = strchr((xdigits = xdigits_u), ch);
		if (pch != NULL) {
			val <<= 4;
			val |= (pch - xdigits);
			if (val > 0xffff)
				return (0);
			saw_xdigit = 1;
			continue;
		}
		if (ch == ':') {
			curtok = src;
			if (!saw_xdigit) {
				if (colonp)
					return (0);
				colonp = tp;
				continue;
			}
			if (tp + INT16SZ > endp)
				return (0);
			*tp++ = (u_char) (val >> 8) & 0xff;
			*tp++ = (u_char) val & 0xff;
			saw_xdigit = 0;
			val = 0;
			continue;
		}
		if (ch == '.' && ((tp + INADDRSZ) <= endp) &&
		    inet_pton4(curtok, tp) > 0) {
			tp += INADDRSZ;
			saw_xdigit = 0;
			break;	/* '\0' was seen by inet_pton4(). */
		}
		return (0);
	}
	if (saw_xdigit) {
		if (tp + INT16SZ > endp)
			return (0);
		*tp++ = (u_char) (val >> 8) & 0xff;
		*tp++ = (u_char) val & 0xff;
	}
	if (colonp != NULL) {
		/*
		 * Since some memmove()'s erroneously fail to handle
		 * overlapping regions, we'll do the shift by hand.
		 */
		const int n = tp - colonp;
		int i;

		for (i = 1; i <= n; i++) {
			endp[- i] = colonp[n - i];
			colonp[n - i] = 0;
		}
		tp = endp;
	}
	if (tp != endp)
		return (0);
	/* bcopy(tmp, dst, IN6ADDRSZ); */
	memcpy(dst, tmp, IN6ADDRSZ);
	return (1);
}

/* !HAVE_INET_PTON */
#endif
