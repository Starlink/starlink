/*
 * E.S.O. - VLT project/ESO Archive
 * "@(#) $Id: RtdRemote.C,v 1.14 1999/03/19 20:09:49 abrighto Exp $"
 *
 * RtdRemote.C - member routines for class RtdRemote, manages remote access 
 *               to RtdImage (server side, see ../../rtdrmt/... for client access)
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  04/03/96  Created
 * Peter W. Draper 13/03/98  Now stores Tcl_File handle. This is to
 *                           work around a bug in OSF/1 Tcl which
 *                           loses this values occasionally. 
 * Allan Brighton  18/03/99  Added #ifdef in RtdRemote.h, since Tcl_File
 *                           is no longer supported in tcl8...
 */
static const char* const rcsId="@(#) $Id: RtdRemote.C,v 1.14 1999/03/19 20:09:49 abrighto Exp $";



#include <string.h>
#include <stdlib.h>
#include <stdio.h>
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
#include "error.h"
#include "define.h"
#include "config.h"
#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif
#include "RtdRemote.h"


// this call changed in tcl8
#if (TCL_MAJOR_VERSION >= 8)
#define RTD_TCL_GETFILE_(x) x
#else
#define RTD_TCL_GETFILE_(x) Tcl_GetFile((void *)x, TCL_UNIX_FD)
#endif

// should be in sys/shm.h
#ifdef NEED_SHM_PROTO
extern "C" void *shmat(int shmid, const void* shmaddr, int shmflg);
extern "C" int shmdt(const void* shmaddr);
#endif

#ifdef NEED_SOCKET_PROTO
// some protos missing in SunOS
extern "C" {
    int socket(int, int, int);
    int connect(int, const void*, int);
    int strncasecmp(char*, char*, int);
    int bind(int s, struct sockaddr *name, int namelen);
    int listen(int s, int backlog);
    int ioctl(int fd, int request, void* arg);
    int select (int width, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, timeval *timeout);
    int accept(int s, sockaddr *addr, int *addrlen);
    void bzero(char *b, int length);
    getsockname(int s, struct sockaddr *name, int *namelen);
}
#endif /* NEED_SOCKET_PROTO */

#ifdef NEED_GETHOSTNAME_PROTO
// these are also missing on solaris 
extern "C" int gethostname(char *name, unsigned int namelen);
#endif /* NEED_GETHOSTNAME_PROTO */

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
	} else if (rc == 0) {
	    if (n == 1)
		return(0);	// EOF, no data read 
	    else
		break;		// EOF, some data was read
	} else
	    return(-1);		// error
    }

    *ptr = 0;
    return(n);
}


/*
 * Write "n" bytes to a descriptor.
 * Use in place of write() when fd is a stream socket.
 */
static int writen(int fd, const char* ptr, unsigned long nbytes)
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
 * write the given buffer to the given file followed by a newline
 */
static int writeline(int fd, char* ptr)
{
    return writen(fd, ptr, strlen(ptr)) + writen(fd, "\n", 1);
}


/*
 * constructor
 */
RtdRemote::RtdRemote(Tcl_Interp* interp, int port, int verbose)
: status_(0),
  socket_(-1),
  interp_(interp),
  verbose_(verbose),
  clientPtr_(NULL)
{
    // clear out client table 
    memset ((char *)&clients_, 0, sizeof(clients_));

    // clear out address structures 
    sockaddr_in addr;	// for local socket address    
    int addrSize = sizeof(addr);
    memset ((char *)&addr, 0, addrSize);

    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons( port );

    // Create the listen socket. 
    socket_ = socket (AF_INET, SOCK_STREAM, 0);
    if (socket_ == -1) {
	status_ = sys_error("socket");
        return;
    }

    // Bind the listen address to the socket. 
    if (bind(socket_, (struct sockaddr *)&addr, addrSize) == -1) {
	status_ = sys_error("bind");
	return;
    }

    // note port and host info in a file for client use
    if ((status_ = makeStatusFile(addr)) != 0)
	return;
	
    // note the port number
    port_ = ntohs( addr.sin_port );

#ifdef DEBUG
    if (verbose_) 
	printf("RtdRemote: using port number %d\n", port_);
#endif
    
    // Initiate the listen on the socket so remote users
    // can connect.  The listen backlog is set to 5. 20
    // is the currently supported maximum.
    if (listen(socket_, 5) == -1) {
	status_ = sys_error("listen");
        return;
    }
    Tcl_CreateFileHandler(RTD_TCL_GETFILE_(socket_),
			  TCL_READABLE, fileEventProc, (ClientData)this);
}


/*
 * destructor 
 */
RtdRemote::~RtdRemote()
{
}


/*
 * create a file $HOME/.rtd-remote containing these values:
 * 
 *    pid hostname port
 *
 * so that the client library can determine how to access this server.
 */
int RtdRemote::makeStatusFile(sockaddr_in& addr)
{
    unsigned int addrSize = sizeof(sockaddr_in);
    if (getsockname(socket_, (struct sockaddr *)&addr, &addrSize) == -1) 
	return sys_error("getsockname");
    
    char filename[1024];
    char* home = getenv("HOME");
    sprintf(filename, "%s/.rtd-remote", (home ? home : "/tmp"));
    FILE* f = fopen(filename, "w+");
    if (!f) 
	return sys_error(filename);
    char hostname[80];
    if (gethostname(hostname, sizeof(hostname)) != 0)
	strcpy(hostname, "localhost");
    fprintf(f, "%u %s %u\n", getpid(), hostname, ntohs(addr.sin_port));
    fclose(f);
    return 0;
}


/*
 * enter client socket in clients table and return the index or
 * -1 for errors
 */
int RtdRemote::enterClient(int sock)
{
    for (int i = 0; i < MAX_CLIENTS; i++) {
	if (clients_[i].socket == 0) {
	    clients_[i].socket = sock;
            clients_[i].handle = RTD_TCL_GETFILE_(sock);
	    clients_[i].thisPtr = this;
	    return i;
	}
    }
    return -1;
}


/*
 * remove the client and close the socket
 */
void RtdRemote::removeClient(int sock)
{
    for (int i = 0; i < MAX_CLIENTS; i++) {
	if (clients_[i].socket == sock) {
	    Tcl_DeleteFileHandler(RTD_TCL_GETFILE_(sock));

#if (TCL_MAJOR_VERSION < 8)
            Tcl_FreeFile( clients_[i].handle );
#endif
	    close(sock);
	    clients_[i].socket = 0;
	    clients_[i].handle = 0;
	    clients_[i].thisPtr = NULL;
	    return;
	}
    }
}


/*
 * This method is called when there is a new connection on the socket
 */
int RtdRemote::fileEvent()
{
    fd_set readMask, readFds;
    FD_ZERO(&readMask);
    FD_SET(socket_, &readMask);
    memcpy(&readFds, &readMask, sizeof(struct fd_set));
    timeval timeout; timeout.tv_sec = timeout.tv_usec = 0;
#ifdef HAVE_SELECT_FD_SET
    int status = select(32, (fd_set *)&readFds, 0, 0, &timeout);
#else
    int status = select(32, (int *)&readFds, 0, 0, &timeout);
#endif
    if (status < 0) 
	return sys_error("select");
    else if (status == 0)
	return TCL_OK;

    if (FD_ISSET(socket_, &readFds) > 0) {
	struct sockaddr_in addr;  // for local socket address
	unsigned int addrSize = sizeof(addr);
	int sock = accept(socket_, (sockaddr *)&addr, &addrSize);
	if (sock < 0) 
	    return sys_error("accept");
	int free = enterClient(sock);
	if (free != -1) {
#ifdef DEBUG
	    if (verbose_)
		printf("RtdRemote: accept on socket: %d, port:%d\n", sock, addr.sin_port);
#endif
	    Tcl_CreateFileHandler(RTD_TCL_GETFILE_(sock),
		TCL_READABLE, clientEventProc, (ClientData)&clients_[free]);
	}
    }

    return TCL_OK;
}


/*
 * send the buffer to the client using the given socket.  The format is
 *
 *      status length\n
 *      result[length]
 *
 * where status is 0 or 1 (the return status of the command), length is
 * the length of the result in bytes and result is the result buffer.
 * status and length are written as ascii integers on a separate line
 * followed by the result on the second line.
 */
int RtdRemote::sendToClient(int socket, int status, int length, const char* result)
{
    char buf[80];
    sprintf(buf, "%d %d\n", status, length);
    if (writen(socket, buf, strlen(buf)) <= 0 
	|| writen(socket, result, length) < 0) {
	return sys_error("error writing to client");
    }
    return 0;
}


/*
 * evaluate the command buf as rtdimage subcommands and return the command's
 * status.
 *
 * XXX Note that some commands should probably not be called from a remote client...
 */
int RtdRemote::evalClientCmd(const char* cmd)
{
    Tcl_ResetResult(interp_);

    // split command into argc/argv...
    int argc = 0;
    char** argv = NULL;
    if (Tcl_SplitList(interp_, (char*)cmd, &argc, &argv) != TCL_OK)
	return TCL_ERROR;
    
    if (argc <= 0) 
	return TCL_OK;		// ignore empty command

    char* name = argv[0];	// command name
    int len = strlen(name);
    argc--;
    argv++;

    // call the RtdImage command method
    // argv[0] is the subcommand name, the rest are the arguments
    if (call(name, len, argc, argv) != TCL_OK)
	return TCL_ERROR;

    return TCL_OK;
}


/*
 * This method is called when there is a message to read from one of the
 * clients. The message should contain
 *
 *   <length><command>
 *
 * where <length> is a binary integer in network byte order, the length
 * of the <command> to read.
 *
 */
int RtdRemote::clientEvent(Client* clientPtr)
{
    clientPtr_ = clientPtr;	// save current client ptr

    if (clientPtr->socket == 0) 
	return TCL_OK;

#ifdef DEBUG
    if (verbose_) 
	printf("RtdRemote: Input on client socket: %d\n",clientPtr->socket);
#endif

    int readable = 0;
    ioctl(clientPtr->socket, FIONREAD, &readable);

#ifdef DEBUG
    if (verbose_)
	printf("RtdRemote: Bytes readable: %d\n",readable);
#endif

    if (readable <= 0) {
	removeClient(clientPtr->socket);
	return TCL_OK;
    }

    char buf[2*1024];
    if (readline(clientPtr->socket, buf, sizeof(buf)) < 0) 
	return sys_error("error reading command from Rtd client");
    
#ifdef DEBUG
    if (verbose_)
	printf("RtdRemote: got: '%s'\n", buf);
#endif

    int status = evalClientCmd(buf);
    return sendToClient(clientPtr->socket, status, strlen(interp_->result), interp_->result);
}
    

/*
 * This static method is called when there is a message to read
 * on the socket. Pass control to the class method.
 */
void RtdRemote::fileEventProc(ClientData clientData, int mask)
{
    RtdRemote* thisPtr = (RtdRemote*)clientData;
    if (thisPtr->fileEvent() != TCL_OK) {
	Tk_BackgroundError(thisPtr->interp_);
    }
}


/*
 * This static method is called when there is a message to read
 * on a client socket. Pass control to the class method.
 */
void RtdRemote::clientEventProc(ClientData clientData, int mask)
{
    RtdRemote::Client* clientPtr = (RtdRemote::Client*)clientData;
    if (! clientPtr) {
	error("no client data");
	return;
    }

    if (clientPtr->thisPtr->clientEvent(clientPtr) != TCL_OK) {
	Tk_BackgroundError(clientPtr->thisPtr->interp_);
    }
}
