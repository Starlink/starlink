/*
 * E.S.O. - VLT project 
 * "@(#) $Id: rtdRemote.c,v 1.3 2005/02/02 01:43:03 brighton Exp $"
 *
 * rtdRemote.c - client library for remote control of an RtdImage
 *               widget, communicates over a socket with a remote
 *               rtdimage widget. See rtdimage/src/RtdRemote.C
 *               for the server side.
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/03/96  Created
 * Peter W. Draper 09/02/98  Removed sys_errlist and replaced with strerror.
 * pbiereic        07/04/04  Fixed: variable argument list
 */
static const char* const rcsId="@(#) $Id: rtdRemote.c,v 1.3 2005/02/02 01:43:03 brighton Exp $";



#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <memory.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <stdarg.h>
#include <errno.h>
#include "rtdRemote.h"


/* -- private interface -- */

/* local struct allocated to manage the client's connection */
typedef struct {
    int socket;			/* socket connection with display */
    int pid;			/* pid of display on host */
    char host[64];		/* hostname where dsplay is running */
    int port;			/* port number to use on host */
    char errmsg[1024];		/* copy of last error message */

    /* optional error handler, to be called with error message */
    RtdRemoteErrorHandler errhandler;
} rtdRemote;

/* note: compiler initializes static data to all 0's, K&R */
static rtdRemote info;	/* this struct holds local info */

 
/* -- local error routines -- */

/*
 * Report the error, syntax like printf.
 * The error message is kept in a local buffer and can be retrieved
 * with rtdRemoteGetError().
 */
static int error(const char *fmt, ...)
{
    va_list args;
    char buf[sizeof(info.errmsg)];

    va_start(args, fmt);
    vsprintf(buf, fmt, args);
    va_end(args);
    
    strcpy(info.errmsg, buf);
    if (info.errhandler)
	(*info.errhandler)(buf);

    return 1;
}


/*
 * report the error, including system error code
 */
static int sys_error(const char *fmt, ...)
{
    va_list args;
    char buf[sizeof(info.errmsg)];
    extern int sys_nerr;
    extern int errno;

    va_start(args, fmt);
    vsprintf(buf, fmt, args);
    va_end(args);
    
    if (errno >= 0 && errno < sys_nerr) {
	strcat(buf, ": ");
	strcat(buf, strerror(errno));
    }

    strcpy(info.errmsg, buf);
    if (info.errhandler)
	(*info.errhandler)(buf);

    return 1;
}


/* -- I/O routines for network I/O taken from the book  --
 *    "UNIX Network Programming" by W. Richard Stevens,
 */


/*
 * Read "n" bytes from a descriptor.
 * Use in place of read() when fd is a stream socket.
 */
static int readn(int fd, char* ptr, int nbytes)
{
    int	nleft, nread;

    nleft = nbytes;
    while (nleft > 0) {
	nread = read(fd, ptr, nleft);
	if (nread < 0)
	    return(nread);		/* error, return < 0 */
	else if (nread == 0)
	    break;			/* EOF */

	nleft -= nread;
	ptr   += nread;
    }
    return(nbytes - nleft);		/* return >= 0 */
}


/*
 * Write "n" bytes to a descriptor.
 * Use in place of write() when fd is a stream socket.
 */
static int writen(int fd, char* ptr, unsigned long nbytes)
{
    int	nleft, nwritten;

    nleft = nbytes;
    while (nleft > 0) {
	nwritten = write(fd, ptr, nleft);
	if (nwritten <= 0)
	    return(nwritten);		/* error */

	nleft -= nwritten;
	ptr   += nwritten;
    }
    return(nbytes - nleft);
}


/*
 * Read the line one byte at a time, looking for the newline.  We store
 * the newline in the buffer, then follow it with a null (the same as
 * fgets(3)).  Not very efficient but usefull for sockets.
 *
 * Returns the number of characters up to, but not including, the null
 * (the same as strlen(3)) or < 0 upon errors.
 */
static int readline(int fd, char* ptr, int maxlen)
{
    int	n, rc;
    char	c;

    for (n = 1; n < maxlen; n++) {
	if ( (rc = read(fd, &c, 1)) == 1) {
	    *ptr++ = c;
	    if (c == '\n')
		break;
	}
	else if (rc == 0) {
	    if (n == 1)
		return(0);	/* EOF, no data read */
	    else
		break;		/* EOF, some data was read */
	} else
	    return(-1);		/* error */
    }

    *ptr = 0;
    return(n);
}


/*
 * write the given buffer to the given file followed by a newline
 */
static int writeline(int fd, char* ptr)
{
    return writen(fd, ptr, strlen(ptr)) + writen(fd, "\n", 1);
}


/* -- other routines -- */


/*
 * read the ~/.rtd-remote file to get the pid, hostname and port number
 * of the RTD, if it is running (check that it is running...)
 */
static int getRtdInfo()
{
    char filename[1024];
    char* home = getenv("HOME");
    FILE* f;
    char hostname[64];

    sprintf(filename, "%s/.rtd-remote", (home ? home : "/tmp"));
    f = fopen(filename, "r");
    if (!f) 
	return error("can't open status file: %s, is the display application running?", filename);
    
    if (fscanf(f, "%u %s %u", &info.pid, info.host, &info.port) != 3)
	return error("error in Rtd status file: %s", filename);
    fclose(f);

    if (kill(info.pid, 0) != 0 
	|| (gethostname(hostname, sizeof(hostname)) == 0 && strcmp(hostname, info.host) != 0))
	return error("display application may not be running on this host?");

    return 0;
}


/* -- public interface -- */


/*
 * write the command to the rtdimage socket and return 0 if all is
 * OK, otherwise 1. "cmd" should not contain a newline, it will be
 * added here.
 */
int rtdRemoteSendOnly(char* cmd) 
{
    if (writeline(info.socket, cmd) <= 0)
	return sys_error("error sending command to RTD");
    return 0;
}


/*
 * open a connection to a currently running Rtd Display. The pid, hostname
 * and port number, if not specified (set to 0) are read from the file 
 * $HOME/.rtd-remote, which is created by the Rtd on startup 
 * (see rtdimage/src/RtdRemote.C).
 *
 * The return value is 0 for success, 1 for error. The error text
 * can be retrieved with rtdRemoteGetError().
 */
int rtdRemoteConnect(int pid, char* host, int port)
{
    struct hostent *hp;		/* pointer to host info */
    struct sockaddr_in addr;	/* for peer socket address */

    if (pid && host && port) {
	info.pid = pid;
	strncpy(info.host, host, sizeof(info.host));
	info.port = port;
    }
    else if (getRtdInfo() != 0) /* get pid, hostname, port from ~/.rtd-remote file */
	return 1;

    /* clear out address */
    memset ((char *)&addr, 0, sizeof(struct sockaddr_in));

    /* Set up the peer address to which we will connect. */
    addr.sin_family = AF_INET;

    /* Get the host information for the rtd display */
    hp = gethostbyname (info.host);
    if (hp == NULL) 
	return sys_error("gethostbyname");

    addr.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
    addr.sin_port = htons(info.port);
    
    /* Create the socket. */
    info.socket = socket(AF_INET, SOCK_STREAM, 0);
    if (info.socket == -1) 
        return sys_error("socket");

    /* Try to connect to the remote Rtd display */
    if (connect(info.socket, (struct sockaddr *)&addr, sizeof(struct sockaddr_in)) == -1) 
        return sys_error("connect");

    return 0;
}


/*
 * disconnect from the Rtd display
 */
void rtdRemoteDisconnect()
{
    if (info.socket != -1) {
	close(info.socket);
	info.socket = -1;
    }
}


/*
 * set a routine to be called with the text of error messages
 * when they occur. The argument is a pointer to an error 
 * handler:
 *
 *       void errhandler(char* msg);
 *
 * The return value is a pointer to the previous error handler, or NULL,
 * if none was defined.
 */
RtdRemoteErrorHandler rtdRemoteSetErrorHandler(RtdRemoteErrorHandler errhandler)
{
    RtdRemoteErrorHandler old_handler = info.errhandler;
    info.errhandler = errhandler;
    return old_handler;
}


/* 
 * Return the text of the most recent error message.
 * 
 */
char* rtdRemoteGetError()
{
    return info.errmsg;
}


/*
 * read the socket with the answer from the last command sent to the
 * remote RtdImage display and return the command's status. The command's
 * result is returned in the "result" argument, which points to local or
 * static storage.
 *
 * The format of the message read from the socket is:
 *
 *    status length\n
 *    msg[length]
 *
 * where status is 0 (OK) or 1 and length is the length of the result that follows.
 */
int rtdRemoteGetResult(int sock, char** result)
{
    static char buf[1024];	/* use to hold results up to 1024 bytes */
    static char* rbuf = buf;	/* may be allocated with malloc if needed */
    static int rbufsize = sizeof(buf) - 1;

    int status;			/* return status of command */
    int length;			/* length of result */

    if (result)
	*result = rbuf;
    buf[0] = '\0';		/* default to empty result */

    if (readline(sock, buf, sizeof(buf)) <= 0)
	return sys_error("error reading result status from rtdimage");

    if (sscanf(buf, "%d %d", &status, &length) != 2)
	return error("unknown result from rtdimage");

    if (length == 0) 
	return status;		/* empty result */

    if (length < 0) 
	return error("bad length received from display application");
    
    if (length >= rbufsize) {	/* use static storage or malloc ? */
	if (rbufsize != sizeof(buf)) 
	    free(rbuf);
	rbuf = (char*)malloc(rbufsize=length+10);
	if (!rbuf) {
	    rbuf = buf;
	    rbufsize = sizeof(buf);
	    return error("rtdRemote: could not allocate %d bytes for result", length);
	}
	if (result)
	    *result = rbuf;
    }
    if (readn(sock, rbuf, length) != length) 
	return sys_error("error reading result from rtdimage");
   
    rbuf[length] = '\0';
    return status;
}


/*
 * Evaluate the given rtdimage subcommand in the remote rtd application
 * and return the status of the command.
 *
 * The command syntax is the same as for the
 * "rtdimage" widget (image type), except that the instance name is
 * missing.  Example:
 *   
 *     char* result;
 *     int status = rtdRemoteCmd("wcscenter", &result);
 *     if (status == 0) {
 * 	 if (sscanf(result, ...) ...) {...}
 *          ...
 *     }
 *
 * On success, "result" points to a char buffer containing the result of
 * the command. The buffer is internal and should not be freed and will
 * be overwritten in the next call to this routine.
 *
 * If the command could not be sent, result is set to a NULL pointer and
 * an error status (1) is returned. The error message can be retrieved
 * with rtdRemoteGetError().
 */
int rtdRemoteSend(char* cmd, char** result)
{
    if (info.socket == -1) 
	return error("no connection to the image display: rtdRemoteConnect was not called");
    
    if (rtdRemoteSendOnly(cmd) != 0)
	return 1;

    return rtdRemoteGetResult(info.socket, result);
}
