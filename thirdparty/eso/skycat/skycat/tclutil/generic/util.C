/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: util.C,v 1.3 1999/03/19 20:10:43 abrighto Exp $" 
 *
 * util.C - utility routines
 * 
 * who             when       what
 * --------------  ---------  ----------------------------------------
 * Allan Brighton  06.Jul.96  Created
 * Peter W. Draper 24.Nov.97  Fixed up suffix to return characters
 *                            after first '.' after last '/' or from
 *                            the beginning of string if no '/'
 *                            available, rather than just last '.'. 
 */
static const char* const rcsId="@(#) $Id: util.C,v 1.3 1999/03/19 20:10:43 abrighto Exp $";


#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <iostream.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "error.h"
#include "define.h"


/*
 * util: make a copy of the given string array in a single buffer
 */
char** copyArray(int len, char** ar) 
{
    int i, n = len * sizeof(char*), m = n;
    char* p;
    for (i = 0; i < len; i++)
	n += strlen(ar[i])+1;

    char** ret = new char*[(n/sizeof(char*))+1];
    p = ((char*)ret) + m;

    for (i = 0; i < len; i++) {
	ret[i] = p;
	strcpy(p, ar[i]);
	p += strlen(ar[i])+1;
    }
    return ret;
}



/* 
 * strip white space from string by returning offset
 * in string and setting trailing white space to '\0'
 */
char* stripWhiteSpace(char* p) 
{
    while(isspace(*p))
	p++;
    char* q = p + strlen(p) - 1;
    while (isspace(*q))
	*q-- = '\0';
    return p;
}


/* 
 * return the suffix of the filename, /xx/xx/xx.xxx or xx.xxx.
 */
const char* fileSuffix(const char* name) 
{
  char *c = (char *) name;
  c = strrchr( c, '/' );
  if ( !c ) {
    c = (char *) name;
    c--;
  }
  c = strchr( ++c, '.' );
  if ( c ) {
    return c + 1;
  } else {
    return "";
  }
}

/* 
 * return the basename of the file (part following last "/", if any)
 */
const char* fileBasename(const char* name) 
{
    const char* p = strrchr(name, '/');
    if (p)
	return p+1;
    return name;
}


/* 
 * return the size of the file in bytes or -1 on error
 */
size_t fileSize(const char* filename) 
{
    struct stat buf;
    if (stat(filename, &buf) != 0) 
	return -sys_error("can't stat ", filename);
    return buf.st_size;
}


/*
 * Get the real name of the given file, which may be the name of a
 * symbolic link. If the file exists, the resolved name is written to the
 * given buffer and returned, otherwise a pointer to the original
 * filename is returned. No error message is generated here.
 */
const char* fileRealname(const char* filename, char* buf, size_t buflen) 
{
    // NOTE: readlink() does NOT null terminate filename !!! 
    int n = readlink(filename, buf, buflen);
    if (n == -1) 
	return filename;
    buf[n] = '\0';		// null terminate filename
    return buf;
}



/*
 * If filename is not an absolute path (starting with '/'), write
 * an absolute path for it to "path". "flag" is set to 1 if path
 * was written to.
 */
int fileAbsPath(const char* filename, char* path, int pathlen, int& flag)
{
    // make sure we use an absolute path
    flag = 0;
    if (filename[0] != '/') {
	if (getcwd(path, pathlen) == NULL) 
	    return sys_error("getcwd");

	strcat(path, "/");
	strcat(path, filename);
	flag = 1;
    }
    return 0;
}



/*
 * Read "n" bytes from a file descriptor.
 * Use in place of read() when fd is a stream socket.
 */
size_t readUnbufferedBytes(int fd, char* ptr, size_t nbytes)
{
    size_t nleft, nread;

    nleft = nbytes;
    while (nleft > 0) {
	nread = read(fd, ptr, nleft);
	if (nread < 0 && errno != EINTR && errno != EAGAIN)
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
 *
 * Taken from Stevens (readline()), "Unix Network Programming"
 */
int readUnbufferedLine(int fd, char* ptr, int maxlen)
{
    int	n;
    size_t rc;
    char	c;

    for (n = 1; n < maxlen; n++) {
	if ( (rc = read(fd, &c, 1)) == 1) {
	    *ptr++ = c;
	    if (c == '\n')
		break;
	    
	} 
	else if (rc == 0) {
	    if (n == 1)
		return(0);	// EOF, no data read 
	    else
		break;		// EOF, some data was read
	} 
	else if (errno != EINTR && errno != EAGAIN)
	    return(-1);		// error
    }

    *ptr = 0;
    return(n);
}


/*
 * Write "n" bytes to a descriptor.  Use in place of write() when fd is a
 * stream socket.
 *
 * Returns the number of characters written or <0 upon error.
 *
 * Taken from Stevens, "Unix Network Programming".
 */
size_t writeUnbufferedBytes(int fd, char* ptr, size_t nbytes)
{
    size_t	nleft, nwritten;

    nleft = nbytes;
    while (nleft > 0) {
	nwritten = write(fd, ptr, nleft);
	if (nwritten < 0  && errno != EINTR && errno != EAGAIN)
	    return(nwritten);	// error 
	else if (nwritten == 0)
	    break;

	nleft -= nwritten;
	ptr   += nwritten;
    }
    return(nbytes - nleft);
}


/*
 * write the given buffer to the given fd followed by a newline
 */
size_t writeUnbufferedLine(int fd, char* ptr)
{
    return writeUnbufferedBytes(fd, ptr, strlen(ptr)) 
	+ writeUnbufferedBytes(fd, "\n", 1);
}


/*
 * Open a socket connection on port on this same host.
 * *socket on connection is returned or set to -1 on failure  
 * The return value is the status (0 is OK).
 */
int localSockConnect(int& sock, int port)
{
    hostent *hp;	// pointer to host info 
    sockaddr_in addr;	// for peer socket address
    sock = -1;          // initial value

    // get local hostname
    struct utsname unameInfo;
    if (uname(&unameInfo) < 0)
	return sys_error("uname failed on localhost?");
    
    /* clear out address */
    memset ((char *)&addr, 0, sizeof(struct sockaddr_in));

    /* Set up the peer address to which we will connect. */
    addr.sin_family = AF_INET;

    /* Get the host information for the local host */
    hp = gethostbyname(unameInfo.nodename);
    if (hp == NULL) 
	return sys_error("failed gethostbyname on localhost?");

    addr.sin_addr.s_addr = ((struct in_addr *)(hp->h_addr))->s_addr;
    addr.sin_port = htons(port);
    
    /* Create the socket */
    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == -1) 
        return sys_error("failed socket on localhost?");

    /* Try to connect to the port */
    if (connect(sock, (struct sockaddr *)&addr, 
		  sizeof(struct sockaddr_in)) == -1) {
	fmt_sys_error("connect failed on port %d", port);
	close(sock); // free the socket fd
	sock = -1;
	return 1;
    }

    return 0;
}



/*
 * Start listening for a socket connection on the given port,
 * or choose a port if port is 0. 
 * The value of sock is set to the socket fd, or -1 on error. 
 * The value of port is set to the port used (may be different if 0). 
 * The return value is the status (0 is OK).
 */
int localSockListen(int& sock, int& port)
{
    // clear out address structures 
    sockaddr_in addr;	// for local socket address    
    size_t addrSize = sizeof(addr);
    memset((char *)&addr, '\0', addrSize);

    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = port;

    // Create the listen socket. 
    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == -1) 
	return sys_error("socket");

    // Bind the listen address to the socket. 
    if (bind(sock, (struct sockaddr *)&addr, addrSize) == -1) 
	return sys_error("bind");

    // note the port number (in case it was 0 and is generated)
    port = addr.sin_port;

    // Initiate the listen on the socket so remote users
    // can connect.  The listen backlog is set to 5. 20
    // is the currently supported maximum.
    if (listen(sock, 5) == -1) 
	return sys_error("listen");
    
    return 0;
}

