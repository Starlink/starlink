/*
*  Name:
*     error.c

*  Purpose:
*     Implement error handling functions.

*  Description:
*     This file implements the Error module which provides functions
*     for handling error conditions in the AST library.  For a
*     description of the module and its interface, see the .h file of
*     the same name.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     2-JAN-1996 (RFWS):
*        Original version.
*     8-JAN-1996 (RFWS):
*        Tidied up.
*     26-JAN-1996 (RFWS):
*        Incorporated changes to prologue style.
*     14-JUN-1996 (RFWS):
*        Added astAt.
*     20-JUN-1996 (RFWS):
*        Added astSetStatus.
*     15-JUL-1996 (RFWS):
*        Sorted out the public interface.
*     16-JUL-1996 (RFWS):
*        Added astWatch.
*     18-MAR-1998 (RFWS):
*        Added notes about functions being available for writing
*        foreign language and graphics interfaces, etc.
*/

/* Define the astCLASS macro (even although this is not a class
   implementation) to obtain access to protected interfaces. */
#define astCLASS

/* Include files. */
/* ============== */
/* Interface definitions. */
/* ---------------------- */
#include "err.h"                 /* Interface to the err module */
#include "error.h"               /* Interface to this module */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stdio.h>

/* Module Variables. */
/* ================= */
/* Status variable. */
static int internal_status = 0;  /* Internal error status */
static int *status_ptr = &internal_status; /* Pointer to status variable */

/* Error context. */
static const char *current_file = NULL; /* Current file name pointer */
static const char *current_routine = NULL; /* Current routine name pointer */
static int current_line = 0;     /* Current line number */

/* Function implementations. */
/* ========================= */
void astAt_( const char *routine, const char *file, int line ) {
/*
*+
*  Name:
*     astAt

*  Purpose:
*     Store a routine, file and line number context in case of error.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "error.h"
*     void astAt( const char *routine, const char *file, int line )

*  Description:
*     This function stores a pointer to two strings containing the
*     names of a routine and a file, together with an integer line
*     number. These values are retained for subsequent use in
*     reporting the context of any error that may arise.

*  Parameters:
*     routine
*        Pointer to a null terminated C string containing a routine
*        name (which should reside in static memory).
*     file
*        Pointer to a null terminated C string containing a file name
*        (which should reside in static memory).
*     line
*        The line number in the file.

*  Notes:
*     - This function returns without action (i.e. without changing
*     the stored values) if the global error status is set. It
*     performs no other error checking.
*     - Any (or all) of the arguments may be omitted by supplying a
*     NULL or zero value (as appropriate) and will then not be included
*     in any error report.
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Store the values supplied. */
   current_routine = routine;
   current_file = file;
   current_line = line;
}

void astClearStatus_( void ) {
/*
c++
*  Name:
*     astClearStatus

*  Purpose:
*     Clear the AST error status.

*  Type:
*     Public macro.

*  Synopsis:
*     #include "error.h"
*     void astClearStatus

*  Description:
*     This macro resets the AST error status to an OK value,
*     indicating that an error condition (if any) has been cleared.

*  Notes:
*     - If the AST error status is set to an error value (after an
*     error), most AST functions will not execute and will simply
*     return without action. Using astClearStatus will restore normal
*     behaviour.
c--
*/

/* Reset the error status value. */
   astSetStatus( 0 );
}

void astError_( int status, const char *fmt, ... ) {
/*
*+
*  Name:
*     astError

*  Purpose:
*     Set the AST error status and report an error message.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "error.h"
*     void astError( int status, const char *fmt, ... )

*  Description:
*     This function sets the AST error status to a specified value and
*     reports an associated error message.

*  Parameters:
*     status
*        The error status value to be set.
*     fmt
*        Pointer to a null-terminated character string containing the
*        format specification for the error message, in the same way
*        as for a call to the C "printf" family of functions.
*     ...
*        Additional optional arguments (as used by e.g. "printf")
*        which specify values which are to appear in the error
*        message.

*  Notes:
*     This function operates just like "printf", except that:
*     - The first argument is an error status.
*     - The return value is void.
*     - A newline is automatically appended to the error message
*     (there is no need to add one).
*     - This function is documented as protected because it should not
*     be invoked by external code. However, it is available via the
*     external C interface so that it may be used when writing (e.g.)
*     foreign language or graphics interfaces.
*-
*/

/* Local Constants: */
#define BUFF_LEN 1023            /* Max. length of an error message */

/* Local Variables: */
   char buff[ BUFF_LEN + 1 ];    /* Message buffer */
   int nc;                       /* Number of characters written */
   va_list args;                 /* Variable argument list pointer */

/* Initialise the variable argument list pointer. */
   va_start( args, fmt );

/* If this is the first report of an error (the global status has not
   previously been set) and error context information is available,
   then construct an error context message. */
   if ( astOK &&
        ( current_routine || current_file || current_line ) ) {
      nc = sprintf( buff, "AST: Error" );
      if ( current_routine ) {
         nc += sprintf( buff + nc, " in routine %s", current_routine );
      }
      if ( current_line ) {
         nc += sprintf( buff + nc, " at line %d", current_line );
      }
      if ( current_file ) {
         nc += sprintf( buff + nc, " in file %s", current_file );
      }
      nc += sprintf( buff + nc, "." );

/* Deliver the error message. */
      astPutErr( status, buff );

/* Set the global status. */
      astSetStatus( status );
   }

/* Write the error message supplied to the formatting buffer. */
   nc = vsprintf( buff, fmt, args );

/* Tidy up the argument pointer. */
   va_end( args );

/* Deliver the error message. */
   astPutErr( status, buff );

/* Set the error status value. */
   astSetStatus( status );

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

int astOK_( void ) {
/*
c++
*  Name:
*     astOK

*  Purpose:
*     Test whether AST functions have been successful.

*  Type:
*     Public macro.

*  Synopsis:
*     #include "error.h"
*     int astOK

*  Description:
*     This macro returns a boolean value (0 or 1) to indicate if
*     preceding AST functions have completed successfully
*     (i.e. without setting the AST error status). If the error status
*     is set to an error value, a value of zero is returned, otherwise
*     the result is one.

*  Returned Value:
*     astOK
*        One if the AST error status is OK, otherwise zero.

*  Notes:
*     - If the AST error status is set to an error value (after an
*     error), most AST functions will not execute and will simply
*     return without action. To clear the error status and restore
*     normal behaviour, use astClearStatus.
c--
*/

/* Test the error status value and return the required result. */
   return ( *status_ptr == 0 );
}

void astSetStatus_( int status ) {
/*
c++
*  Name:
*     astSetStatus

*  Purpose:
*     Set the AST error status to an explicit value.

*  Type:
*     Public function.

*  Synopsis:
*     #include "error.h"
*     void astSetStatus( int status )

*  Description:
*     This function sets the AST error status to the value supplied.
*     It does not cause any error message to be produced and should
*     not be used as part of normal error reporting. Its purpose is
*     simply to communicate to AST that an error has occurred in some
*     other item of software.
*
*     For example, a source or sink function supplied as an argument
*     to astChannel or astFitsChan might use this to signal that an
*     input/output error has occurred. AST could then respond by
*     terminating the current read or write operation.

*  Parameters:
*     status
*        The new error status value to be set.

*  Notes:
*     - If the AST error status is set to an error value, most AST
*     functions will not execute and will simply return without
*     action. To clear the error status and restore normal behaviour,
*     use astClearStatus.
c--
*/

/* Set the error status value. */
   *status_ptr = status;
}

int astStatus_( void ) {
/*
c++
*  Name:
*     astStatus

*  Purpose:
*     Obtain the current AST error status value.

*  Type:
*     Public macro.

*  Synopsis:
*     #include "error.h"
*     int astStatus

*  Description:
*     This macro returns the current value of the AST error status.

*  Returned Value:
*     astStatus
*        The AST error status value.

*  Notes:
*     - If the AST error status is set to an error value (after an
*     error), most AST functions will not execute and will simply
*     return without action. To clear the error status and restore
*     normal behaviour, use astClearStatus.
c--
*/

/* Return the error status value. */
   return *status_ptr;
}

int *astWatch_( int *status_address ) {
/*
c++
*  Name:
*     astWatch

*  Purpose:
*     Identify a new error status variable for the AST library.

*  Type:
*     Public function.

*  Synopsis:
*     #include "error.h"
*     int *astWatch( int *status_address )

*  Description:
*     This function allows a new error status variable to be accessed
*     by the AST library when checking for and reporting error
*     conditions.
*
*     By default, the library uses an internal integer error status
*     which is set to an error value if an error occurs. Use of
*     astWatch allows the internal error status to be replaced by an
*     integer variable of your choosing, so that the AST library can
*     share its error status directly with other code which uses the
*     same error detection convention.
*
*     If an alternative error status variable is supplied, it is used
*     by all related AST functions and macros (e.g. astOK, astStatus
*     and astClearStatus).

*  Parameters:
*     status_address
*        Address of an int whose value is to be used subsequently as
*        the AST error status. If a NULL pointer is supplied, the AST
*        library will revert to using its own internal error status.

*  Returned Value:
*     astWatch()
*        Address of the previous error status variable. This may later
*        be passed back to astWatch to restore the previous behaviour
*        of the library. (Note that on the first invocation of
*        astWatch the returned value will be the address of the
*        internal error status variable.)

*  Notes:
*     - This function is not available in the FORTRAN 77 interface to
*     the AST library.
c--
*/

/* Local Variables: */
   int *result;                  /* Value to be returned */

/* Save the old status variable address. */
   result = status_ptr;

/* Replace it with the new one. */
   status_ptr = status_address ? status_address : &internal_status;

/* Return the old address. */
   return result;
}
