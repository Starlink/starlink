#ifndef _error_h_
#define _error_h_
/*
 * E.S.O. - VLT project 
 * "@(#) $Id: error.h,v 1.2 2005/02/02 01:43:01 brighton Exp $" 
 *
 * error.h - declarations for global error reporting
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */

/* error status */
#undef OK
#undef ERROR
enum {OK, ERROR};

/* report the error */
int error(const char* msg1, const char* msg2="", int code = 0);

/* report the error with the system error code, like perror */
int sys_error(const char* msg1, const char* msg2="");

/* report error with a printf style interface */
int fmt_error(const char* fmt, ...);

/* report a system error with a printf style interface */
int fmt_sys_error(const char* fmt, ...);

/* return the text of the previous error message */
char* last_error();

/* return the error code for the previous error */
int last_error_code();

/* reset the last_error buf to empty */
void clear_error();

/* set a routine to be called when errors occur and return ptr to previous handler */
void (*set_error_handler(void (*error_handler)(const char*)))(const char*);

/* simple error handler: print the message */
void print_error(const char* msg);

/* print a message (like printf) */
void log_message(const char* fmt, ...);

/* set a routine to be called for log messages and return ptr to previous handler */
void (*set_log_message_handler(void (*message_handler)(const char*)))(const char*);

/* simple message handler: print the message */
void print_log_message(const char* msg);



#endif /* _error_h_ */

