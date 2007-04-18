// -*-c++-*-
#ifndef _util_h_
#define _util_h_
/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: util.h,v 1.1.1.1 2006/01/12 16:41:04 abrighto Exp $" 
 *
 * util.h - utility routines
 * 
 * who             when       what
 * --------------  ---------  ----------------------------------------
 * Allan Brighton  06.Jul.96  Created
 */


// util: make a copy of the given string array in a single buffer
char** copyArray(int len, char** ar);

// strip white space from string by returning offset
// in string and setting trailing white space to '\0'
char* stripWhiteSpace(char* p);

// return the suffix of the string (part following ".", if any)
const char* fileSuffix(const char* p);

// return the basename of the file (part following last "/", if any)
const char* fileBasename(const char* p);

// get the real name of a file, which may be a link
const char* fileRealname(const char* filename, char* buf, size_t buflen);

// return the size of the file in bytes or -1 on error
int fileSize(const char* filename);

// If filename is not an absolute path (starting with '/'), write
// an absolute path for it to "path". Sets flag to 1 if path was changed.
int fileAbsPath(const char* filename, char* path, int pathlen, int& flag);

// Read the line one byte at a time (for sockets and pipes, stdin, ...)
int readUnbufferedLine(int fd, char* ptr, int maxlen);

// Read "n" bytes from a file descriptor.
int readUnbufferedBytes(int fd, char* ptr, int nbytes);

// write the given buffer to the given fd followed by a newline
int writeUnbufferedLine(int fd, char* ptr);

// Write "n" bytes to a descriptor fd (for sockets and pipes, stdin,...)
int writeUnbufferedBytes(int fd, char* ptr, int nbytes);

// Open a socket connection on port on this same host.
int localSockConnect(int& sock, int port);

// start listening on the given port (or choose port, if 0)
int localSockListen(int& sock, int& port);

#endif /* _util_h_ */
