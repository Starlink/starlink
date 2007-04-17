/*************************************************************************
* E.S.O. - VLT project
* "@(#) $Id: rtdImageEvent.c,v 1.2 2006/01/18 18:31:56 abrighto Exp $"
* rtdImageEvent.c
*
* who            when      what
* ---------      --------  ----------------------------------------------
* T.Herlin       11/05/94  Created
* A.Brighton     05/02/96  add htons calls after reported problems on OSF machines
* D.Hopkinson    01/04/97  add cleanup of semaphores on skipped images
* pbiereic       16/06/97  add setsockopt(..SO_REUSEADDR..) in rtdInitServer
* J.Stegmeier    19/03/99  Added TCP_NODELAY for faster image update
* pbiereic       01/03/01  Added: rtdServerPing(), rtdSleep()
*/
/************************************************************************
*   NAME
*    rtdImageEvent     - Real-Time image event client interface.
*
*    rtdInitImageEvt   - initialize and register to rtdServer.
*
*    rtdSendImageInfo  - send image event information to rtdServer.
*
*    rtdAttachImageEvt - attach to image event notification.
*
*    rtdDetachImageEvt - detach notification of image events.
*
*    rtdRecvImageInfo  - receive image event information from rtdServer.
*
*    int rtdServerPing - "ping" the rtdServer
*
*    rtdClose          - close event handel.
*    
*   SYNOPSIS
*   #include "rtdImageEvent.h"
*   int rtdInitImageEvt(char              *requestor,
*                       rtdIMAGE_EVT_HNDL *eventHndl,
*                       char              *error)
*
*   int rtdSendImageInfo(rtdIMAGE_EVT_HNDL  *eventHndl,
*                        rtdIMAGE_INFO      *imageInfo,
*                        char               *error)
* 
*   int rtdAttachImageEvt(rtdIMAGE_EVT_HNDL *eventHndl,
*                         char              *camera,
*                         char              *error)
*
*   int rtdDetachImageEvt(rtdIMAGE_EVT_HNDL *eventHndl,
*                         char              *camera,
*                         char              *error)
*
*   int rtdRecvImageInfo(rtdIMAGE_EVT_HNDL *eventHndl,
*                        rtdIMAGE_INFO     *imageInfo,
*                        int                verbose,
*                        char              *error)
*
*   int rtdServerPing(rtdIMAGE_EVT_HNDL *eventHndl,
*           		 char              *error)
*
*   int rtdClose(rtdIMAGE_EVT_HNDL *eventHndl,
*                char              *error)
*
*   DESCRIPTION
*
*   rtdInitImageEvt() registers the current process e.g. image
*   aquisition process or rtdWidget to the rtdServer running on 
*   the local workstation. The requestor is a string passed to identify
*   the process. The function returns with a valid event handle
*   which is used for subsequent calls to the rtdServer.
* 
*   rtdSendImageInfo() is used to send image event information
*   when an image is ready to be displayed in shared memory.
*   The eventHndl is the one passed from rtdInitImageEvt,
*   the imageInfo is information about the image.
* 
*   rtdAttachImageEvt() attaches a process to event notification of
*   an image source. eventHndl is the handle returned by rtdRecvImageInfo.
*   camera is the name of the system providing images e.g. aquisition 
*   system. After an attach the received image events can be retrieved
*   by a call to rtdRecvImageInfo().
*
*   rtdDetachImageEvt() stops the notification of image events. eventHndl
*   is the handle returned by rtdRecvImageInfo. camera is the name of the 
*   system providing images.
*
*   rtdRecvImageInfo() is used to receive the image event information
*   from the rtdServer. Image events are received when the process is
*   attached to event notification. eventHndl is the handle returned 
*   by rtdInitImageEvt, imageInfo a pointer to a rtdIMAGE_INFO structure.
*   If verbose is non-zero diagnostic messages are printed.
*   
*   rtdServerPing() is used by RTD to check that the rtdServer is still
*   responding.
*
*   rtdClose()
*   Closes connection to rtdServer. Use when finished with real-time display
*   or repeated errors occuring on rtdSendImageInfo.
*
*   RETURN VALUES
*
*   RTD_OK upon success or
*   RTD_ERROR upon failure.
*
*   NOTE 
*
*   The error field in all functions is reserved for future use.
* 
*   ENVIRONMENT
*
*   The port number of rtdServer is normally specified in /etc/services.
*   If the user want to use a different port number the this can be set
*   in the environment RTD_SERVER_PORT.
*
*   EXAMPLE
*
*   // sample application which send a SHORT image to real-time display
*   #include <sys/ipc.h>
*   #include <sys/shm.h>
*   #include "rtdImageEvent.h"
*
*   rtdIMAGE_EVT_HNDL  eventHndl;
*   rtdIMAGE_INFO      imageInfo;
*   char               *errMsg;
*   int                shmId;
*   char               *shmPtr;
*
*   if (rtdInitImageEvt("My_CCD_Camera",&eventHndl,errMsg) == RTD_ERROR)
*	{
*	fprintf(stderr,"rtdInitImageEvt error:%s",errMsg);
*	... handle error ...
*	}
*  
*   shmId    = shmget(IPC_PRIVATE,512*512*sizeof(short),0666); 
*
*    shmPtr   = (char *)shmat(shmId,NULL,0); 
*   if (shmPtr == -1)
*       { .. handle error ... }
* 
*   ... generate the image ...
*
*   memset(&imageInfo, '\0', sizeof(rtdIMAGE_INFO));
*   imageInfo.dataType = SHORT;
*   imageInfo.shmId    = shmId;
*   imageInfo.xPixels  = 512;
*   imageInfo.yPixels  = 512;
*
*  // send image event  
*  if (rtdSendImageInfo(&eventHndl,&imageInfo,errMsg) == RTD_ERROR)
*	{
*	fprintf(stderr,"rtdSendImageInfo error:%s",errMsg);
*	... handle error ...
*	}
*
*  // if finishing close connection and delete shared memory
*  rtdClose((&eventHndl,errMsg); 
*
*  if (shmId) shmctl(shmId,IPC_RMID,NULL);
*
*   WARNINGS
*       If you are not using semaphore locking then set semId=-1 in the
*       image event structure (see rtdImageEvent.h). Since semId=0 is
*       a valid number it can happen that rtdServer decrements a
*       semaphore created by another process which can lead to serious
*       problems!
*
*   SEE ALSO
*
*   rtdServer(1)
*
*-------------------------------------------------------------------------
*/
static const char* const rcsId="@(#) $Id: rtdImageEvent.c,v 1.2 2006/01/18 18:31:56 abrighto Exp $";


/*
 * System Headers
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <memory.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <netdb.h>

/*
 * Local Headers
 */
#include "rtdImageEvent.h"

int rtdInitImageEvt(char              *requestor,
		    rtdIMAGE_EVT_HNDL *eventHndl,
		    char              *error)
{
    char *subr = "rtdInitImageEvt";
    int s;                          /* connected socket descriptor */
    struct hostent *hp;             /* pointer to host info for remote host */
    struct servent *sp;             /* pointer to service information */
    char *ctime();                  /* declare time formatting routine */
    
    struct sockaddr_in rtdClientAddr;  /* for local socket address */
    struct sockaddr_in rtdServerAddr;  /* for peer socket address */
    socklen_t addrlen;
    int optval;

    char buf[256];

    /* clear out address structures */
    memset ((char *)&rtdClientAddr, 0, sizeof(struct sockaddr_in));
    memset ((char *)&rtdServerAddr, 0, sizeof(struct sockaddr_in));

    /* check input parameters */
    if (eventHndl == NULL) return RTD_ERROR;
    
    /* This version only supports the local host connection */
    gethostname(buf,sizeof(buf));
    
    /* Set up the peer address to which we will connect. */
    rtdServerAddr.sin_family = AF_INET;
    /* Get the host information for the hostname that the
     * user passed in.
     */
    hp = gethostbyname (buf);
    
    if (hp == NULL) {
	rtdSetError(subr, error, RTD_ERR_GETHOSTNAME);
	return RTD_ERROR;
    }

    rtdServerAddr.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;

    /* Assign the rtdServer port number:
     * 1: check the environment RTD_SERVER_PORT 
     * 2: else read in the /etc/services
     * 3: if everything fails try the fallback RTD_FALLBACK_PORT
     */
    if (getenv(RTD_SERVER_PORT))
	rtdServerAddr.sin_port = htons(atoi(getenv(RTD_SERVER_PORT)));
    
    /* Find the information for the rtdServer
     * in order to get the needed port number.
     */
    if (rtdServerAddr.sin_port == 0) {
	sp = getservbyname (RTD_SERVICE, "tcp");
	if (sp != NULL)
	    rtdServerAddr.sin_port = sp->s_port;
	else {
	    /*fprintf(stderr, "%s: service not found ",RTD_SERVICE); */
	    rtdServerAddr.sin_port = htons(RTD_FALLBACK_PORT);
	}
    }
    /* Create the socket. */
    s = socket (AF_INET, SOCK_STREAM, 0);
    if (s == -1) {
	rtdSetError(subr, error, RTD_ERR_CREAT_SOCKET);
	return RTD_ERROR;
    }

    optval = 1;
    setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (const char *)&optval, sizeof optval);

    /* Try to connect to the remote server at the address
     * which was just built into rtdServerAddr.
     */
    if (connect(s, (struct sockaddr *)&rtdServerAddr, sizeof(struct sockaddr_in)) == -1) {
	rtdSetError(subr, error, RTD_ERR_CONNECT_SOCKET);
	close(s);
	return RTD_ERROR;
    }
    /*
     */
    addrlen = sizeof(struct sockaddr_in);
    if (getsockname(s, (struct sockaddr*)&rtdClientAddr, &addrlen) == -1) {
	close(s);
	return RTD_ERROR;
    }
    /*
      printf("Connected to %s on port %u at %s",
      RTD_SERVICE, ntohs(rtdClientAddr.sin_port), ctime(&timevar));
    */

    /* Insert event handle data */
    eventHndl->socket = s;
    memcpy(&eventHndl->clientAddr,&rtdClientAddr,sizeof(rtdClientAddr));
    strncpy(eventHndl->reqName,requestor,RTD_NAMELEN);

    return RTD_OK;
}

/*
 * rtdSetError prints an error message on stdout if the error pointer
 * is NULL; otherwise it just returns. See also the NOTE - which says
 * that the "error field" is reserved for the future.
 */
void rtdSetError(char *subr, char *error, char *msg)
{
    if (error)
	error = msg;
    else
	fprintf(stderr,"%s:%s !\n",subr, msg);
}

int rtdInitServer(int  *listenSocket,
		  int  portNumber,
		  char *error)
{
    char *subr = "rtdInitServer";
    int ls;                            /* listen socket descriptor       */
    struct servent *sp;                /* pointer to service information */
    struct sockaddr_in rtdServerAddr;  /* for local socket address       */
    int optval;

    /* check output parameters */
    if (listenSocket == NULL) {
	rtdSetError(subr, error, RTD_ERR_NULL_PTR);
	return RTD_ERROR;
    }
    /* clear out address structures */
    memset ((char *)&rtdServerAddr, 0, sizeof(struct sockaddr_in));

    rtdServerAddr.sin_family = AF_INET;
    rtdServerAddr.sin_addr.s_addr = INADDR_ANY;

    /* Assign the rtdServer port number:
     * 1: check if portnumber set by user
     * 2: else read in the /etc/services
     * 3: if everything fails try the fallback RTD_FALLBACK_PORT
     */
    if (portNumber != 0)
	rtdServerAddr.sin_port = htons(portNumber);
	
    if (rtdServerAddr.sin_port == 0) {
	sp = getservbyname (RTD_SERVICE, "tcp");
	if (sp != NULL)
	    rtdServerAddr.sin_port = sp->s_port;
	else
	    rtdServerAddr.sin_port = htons(RTD_FALLBACK_PORT);
    }

    /* Create the listen socket. */
    ls = socket (AF_INET, SOCK_STREAM, 0);
    if (ls == -1) {
	rtdSetError(subr, error, RTD_ERR_CREAT_SOCKET);
	return RTD_ERROR;
    }

    /* set socket options (for HP set all bytes to 1!) */
    optval = 0x1111;
    setsockopt(ls, SOL_SOCKET, SO_REUSEADDR, (const char *)&optval, sizeof optval);

    /* Bind the listen address to the socket. */
    if (bind(ls, (struct sockaddr *)&rtdServerAddr, sizeof(struct sockaddr_in)) == -1) {
	rtdSetError(subr, error, RTD_ERR_BIND_SOCKET);
	return RTD_ERROR;
    }

    /* Initiate the listen on the socket so remote users
     * can connect.  The listen backlog is set to 5. 20
     * is the currently supported maximum.
     */
    if (listen(ls, 5) == -1) {
	rtdSetError(subr, error, RTD_ERR_LISTEN_SOCKET);
	return RTD_ERROR;
    }

    *listenSocket = ls;
    return RTD_OK;
}

/* 
 * Write nbyte to socket. If the pipe is broken (i.e. when rtdServer was
 * killed) ignore the signal for backwards compatibility, so that the
 * client can continue.
 */
int rtdWrite(int fd, void* buf, int nbyte)
{
    signal(SIGPIPE, SIG_IGN);
    return write(fd, buf, nbyte);
}

int rtdSendImageInfo(rtdIMAGE_EVT_HNDL  *eventHndl,
		     rtdIMAGE_INFO      *imageInfo,
		     char               *error)
{
    char *subr = "rtdSendImageInfo";

    static rtdPACKET *rtdPacket;
    
    /* check input parameters */
    if (eventHndl == NULL || imageInfo == NULL) {
	rtdSetError(subr, error, RTD_ERR_NULL_PTR);
	return RTD_ERROR;
    }
    
    if (rtdPacket == NULL)
	rtdPacket = malloc(sizeof(rtdPACKET));
    
    if (eventHndl->socket == 0) {
	rtdSetError(subr, error, RTD_ERR_NO_SOCKET);
	return RTD_ERROR;
    }
    
    /* setup protocol packet */
    rtdPacket->opcode = IMAGEINFO;
    rtdPacket->body.data.hdr.reqType = IMAGETRANS; /* IT SW */
    strncpy(rtdPacket->body.data.hdr.reqName,eventHndl->reqName, RTD_NAMELEN);
    memcpy(&rtdPacket->body.data.rtdImageInfo, imageInfo, sizeof(rtdIMAGE_INFO));
    rtdPacket->body.data.rtdImageInfo.version = RTD_EVT_VERSION;

    /* use unbuffered write operation */
    if (rtdWrite(eventHndl->socket, rtdPacket,sizeof(rtdPACKET)) != sizeof(rtdPACKET)) {
	rtdSetError(subr, error, RTD_ERR_DATAWRITE);
	return RTD_ERROR;
    }
    return RTD_OK;
}


/*
 * read a message from the rtd server and return it in imageInfo.
 * - "eventHndl" is the handle initialized by  rtdInitImageEvt.
 * - if "verbose" is non-zero, print diagnostic messages to stdout.
 * - "error" is reserved for future use.
 */
int rtdRecvImageInfo(rtdIMAGE_EVT_HNDL *eventHndl,
                     rtdIMAGE_INFO     *imageInfo,
		     int                verbose,
		     char              *error)
{
    char *subr = "rtdRecvImageInfo";
    rtdPACKET rtdPacket;
    long       nbytes = 0;
    int        n = 0;

    /* check input parameters */
    if (eventHndl == NULL || imageInfo == NULL) {
	rtdSetError(subr, error, RTD_ERR_NULL_PTR);
	return RTD_ERROR;
    }
    
    if (eventHndl->socket == 0) {
	rtdSetError(subr, error, RTD_ERR_NO_SOCKET);
	return RTD_ERROR;
    }
    
    while (1) {
	/* If there is more than one message to read, skip all but the last unless
	 * the shm buffer is locked */
	if (ioctl(eventHndl->socket, FIONREAD, &nbytes) != 0) {
	    if (verbose)
		rtdSetError(subr, error, "rtdRecvImageInfo: ioctl failed\n");
	    return RTD_ERROR;
	}

	if (nbytes == 0)
	    break;

	memset(&rtdPacket,'\0',sizeof(rtdPACKET));
    
	n = read(eventHndl->socket, &rtdPacket, sizeof(rtdPACKET));
	if (n < 0) {
	    rtdSetError(subr, error, strerror(errno));
	    return RTD_ERROR;
	}

	if (n == sizeof(rtdPACKET)) {
	    if (rtdPacket.body.data.rtdImageInfo.semId) {
		break;
	    }
	}
	if (nbytes > sizeof(rtdPACKET)) {
	    if (verbose)
		printf("%s: ignoring unread packets\n", subr);
	} else
	    break;
    }

    if (n < 32) /* hardcoded! We need at least the info struct until binningY */ {
	rtdSetError(subr, error, RTD_ERR_UNKNOWN_SIZE);
	return RTD_ERROR;
    }

    if (rtdPacket.body.data.rtdImageInfo.version != RTD_EVT_VERSION)
	rtdSetError(subr, error, RTD_ERR_INCOMPAT);

    /* setup protocol packet */
    if (rtdPacket.opcode == IMAGEINFO || rtdPacket.body.data.hdr.reqType == IMAGETRANS) {
	memcpy(imageInfo, &rtdPacket.body.data.rtdImageInfo, sizeof(rtdIMAGE_INFO));
	return RTD_OK; 
    }

    rtdSetError(subr, error, RTD_ERR_UNKNOWN_OPCODE);
    return RTD_ERROR;
}

int rtdAttachImageEvt(rtdIMAGE_EVT_HNDL *eventHndl,
                      char              *camera,
		      char              *error)
{
    char *subr = "rtdAttachImageEvt";   
    rtdPACKET rtdPacket;

    memset(&rtdPacket,'\0',sizeof(rtdPACKET));
    
    /* check input parameters */
    if (eventHndl == NULL || camera == NULL) {
	rtdSetError(subr, error, RTD_ERR_NULL_PTR);
	return RTD_ERROR;
    }

    if (eventHndl->socket == 0) {
	rtdSetError(subr, error, RTD_ERR_NO_SOCKET);
	return RTD_ERROR;
    }
    
    /* setup protocol packet */
    rtdPacket.opcode = ATTACH;
    rtdPacket.body.data.hdr.reqType = RTDWIDGET; /* RTD SW */
    strncpy(rtdPacket.body.data.hdr.reqName,eventHndl->reqName,
            RTD_NAMELEN);
    strncpy(rtdPacket.body.data.hdr.camName,camera,
            RTD_NAMELEN);
    
    /* use unbuffered write operation */
    if (rtdWrite(eventHndl->socket, &rtdPacket,sizeof(rtdPACKET)) != sizeof(rtdPACKET)) {
	rtdSetError(subr, error, RTD_ERR_DATAWRITE);
	return RTD_ERROR;
    }

    return RTD_OK;
}

int rtdDetachImageEvt(rtdIMAGE_EVT_HNDL *eventHndl,
                      char              *camera,
		      char              *error)
{
    char *subr = "rtdDetachImageEvt";
    rtdPACKET rtdPacket;

    memset(&rtdPacket,'\0',sizeof(rtdPACKET));
    
    /* check input parameters */
    if (eventHndl == NULL) {
	rtdSetError(subr, error, RTD_ERR_NULL_PTR);
	return RTD_ERROR;
    }
    
    if (eventHndl->socket == 0) {
	rtdSetError(subr, error, RTD_ERR_NO_SOCKET);
	return RTD_ERROR;
    }
    
    /* setup protocol packet */
    rtdPacket.opcode = DETACH;
    rtdPacket.body.data.hdr.reqType = RTDWIDGET; /* RTD SW */
    strncpy(rtdPacket.body.data.hdr.reqName,eventHndl->reqName, RTD_NAMELEN);

    strncpy(rtdPacket.body.data.hdr.camName,camera, RTD_NAMELEN);
    
    /* use unbuffered write operation */
    if (rtdWrite(eventHndl->socket, &rtdPacket,sizeof(rtdPACKET)) != sizeof(rtdPACKET)) {
	rtdSetError(subr, error, RTD_ERR_DATAWRITE);
	return RTD_ERROR;
    }

    return RTD_OK;
}

int rtdServerPing(rtdIMAGE_EVT_HNDL *eventHndl,
		  char              *error)
{
    char *subr = "rtdServerPing";
    rtdPACKET rtdPacket;

    memset(&rtdPacket,'\0',sizeof(rtdPACKET));
    
    /* check input parameters */
    if (eventHndl == NULL) {
	rtdSetError(subr, error, RTD_ERR_NULL_PTR);
	return RTD_ERROR;
    }
    
    if (eventHndl->socket == 0) {
	rtdSetError(subr, error, RTD_ERR_NO_SOCKET);
	return RTD_ERROR;
    }
    
    /* setup protocol packet */
    rtdPacket.opcode = PING;
    rtdPacket.body.data.hdr.reqType = RTDWIDGET; /* RTD SW */
    strncpy(rtdPacket.body.data.hdr.reqName,eventHndl->reqName, RTD_NAMELEN);
    
    /* use unbuffered write operation */
    if (rtdWrite(eventHndl->socket, &rtdPacket,sizeof(rtdPACKET)) != sizeof(rtdPACKET)) {
	rtdSetError(subr, error, RTD_ERR_DATAWRITE);
	return RTD_ERROR;
    }

    return RTD_OK;
}

int rtdClose(rtdIMAGE_EVT_HNDL *eventHndl,
             char              *error)
{
    /* check input parameters */
    if (eventHndl == NULL || eventHndl->socket == 0) {
	return RTD_OK;
    }

    /* simply close the connection */
    close(eventHndl->socket);
    eventHndl->socket = 0;
    return RTD_OK;
}

void rtdSleep(int msec) 
{
    struct timeval time;
    time.tv_sec  = msec / 1000;
    time.tv_usec = (msec % 1000) * 1000;
    select(0, (fd_set *) 0, (fd_set *) 0, (fd_set *) 0, &time);
}
