/*
 * E.S.O. - VLT project 
 * "@(#) $Id: error.C,v 1.2 1998/02/18 11:33:05 abrighto Exp $"
 *
 * error.C - error reporting routines
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */
static const char* const rcsId="@(#) $Id: error.C,v 1.2 1998/02/18 11:33:05 abrighto Exp $";


#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <iostream.h>
#include <strstream.h>
#include <errno.h>
#include <stdio.h>
#include "error.h"

// static variable holding text of last error messages
static char errmsg_[5*1024];

// error code (see defines in <sys/errno.h> for values)
static int errno_ = 0;

// optional error handler, to be called with error messages
static void (*errhandler_)(const char*) = NULL;

// optional message handler, to be called with log messages
static void (*msghandler_)(const char*) = NULL;


/*
 * global error reporting routine
 */
int error(const char* msg1, const char* msg2, int code)
{
    char buf[sizeof(errmsg_)];
    ostrstream os(buf, sizeof(buf));
    os << msg1 << msg2 << ends;
    
    if (errhandler_)
	(*errhandler_)(buf);
    else
	print_error(buf);

#ifdef XXXDEBUG
    cerr << "debug: " << buf << endl;
#endif

    errno_ = code;
    strncpy(errmsg_, buf, sizeof(errmsg_)-1);
    return ERROR;
}


/*
 * report the error, including system error code
 */
int sys_error(const char* msg1, const char* msg2)
{
    extern int sys_nerr;
    extern char *sys_errlist[];
    extern int errno;
    char* s = strerror(errno);

    // XXX configure: sys_nerr may not be defined ?
    if (s == NULL || errno < 0 || errno >= sys_nerr)
	return error(msg1, msg2);

    char buf[sizeof(errmsg_)];
    ostrstream os(buf, sizeof(buf));
    os << msg1 << msg2 << ": " << (s ? s : sys_errlist[errno]) << ends;

    if (errhandler_)
	(*errhandler_)(buf);
    else
	print_error(buf);

#ifdef XXXDEBUG
    cerr << "debug: " << buf << endl;
#endif

    errno_ = errno;
    strcpy(errmsg_, buf);
    return ERROR;
}


/*
 * This routine has an interface like printf and reports an error in the
 * same way as error() above.
 */
int fmt_error(const char* fmt, ...)
{
    char buf[1024];
    va_list ap;
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    
    return error(buf);
}


/*
 * This routine has an interface like printf and reports a system error
 * in the same way as sys_error() above.
 */
int fmt_sys_error(const char* fmt, ...)
{
    char buf[1024];
    va_list ap;
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    
    return sys_error(buf);
}


/* 
 * return the text of the previous error message
 */
char* last_error() 
{
    return errmsg_;
}


/* 
 * return the error code for the previous error
 */
int last_error_code() 
{
    return errno_;
}


/* 
 * reset the last_error buf to empty 
 */
void clear_error()
{
    errmsg_[0] = '\0';
}


/*
 * set a routine to be called with the text of error messages
 * when they occur. The argument is a pointer to an error 
 * handler:
 *
 *       void errhandler(const char* msg);
 *
 * The return value is a pointer to the previous error handler, or NULL,
 * if none was defined.
 */
void (*set_error_handler(void (*errhandler)(const char*)))(const char*)
{
    void (*old_handler)(const char*) = errhandler_;
    errhandler_ = errhandler;
    return old_handler;
}


/*  
 * print the given message on stderr (may be used as an error
 * handler)
 */
void print_error(const char* msg)
{
    fprintf(stderr, "%s\n", msg);
    fflush(stderr);
}


/* ---- The routines below are for log messages that are not errors --- */



/*
 * This routine logs a message and has an interface like printf.
 */
void log_message(const char* fmt, ...)
{
    char buf[1024];
    va_list ap;
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    
    if (msghandler_)
	(*msghandler_)(buf);
    else 
	print_log_message(buf);
}

/*
 * set a routine to be called with the text of log messages
 * when they are made. The argument is a pointer to a log message 
 * handler:
 *
 *       void msghandler(const char* msg);
 *
 * The return value is a pointer to the previous message handler, or NULL,
 * if none was defined.
 */
void (*set_log_message_handler(void (*msghandler)(const char*)))(const char*)
{
    void (*old_handler)(const char*) = msghandler_;
    msghandler_ = msghandler;
    return old_handler;
}


/*  
 * print the given message on stdout (may be used as a message
 * handler)
 */
void print_log_message(const char* msg)
{
    printf("%s\n", msg);
    fflush(stdout);
}

