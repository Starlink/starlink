#if !defined( ERROR_INCLUDED )   /* Include this file only once */
#define ERROR_INCLUDED 1
/*
*+
*  Name:
*     error.h

*  Purpose:
*     Define the interface to the Error module.

*  Description:
*     This module defines functions which implement error handling and
*     reporting of error messages from within the AST library. A
*     simple public interface is included to allow the AST error
*     status to be tested and cleared after an error.
*
*     Note that this module is not a class implementation, although it
*     resembles one.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S Berry (Starlink)
*     NG: Norman Gray (Starlink)

*  History:
*     2-JAN-1996 (RFWS):
*        Original version.
*     26-JAN-1996 (RFWS):
*        Added function interfaces.
*     14-JUN-1996 (RFWS):
*        Added AST__FAC and astAt.
*     20-JUN-1996 (RFWS):
*        Added astSetStatus.
*     16-JUL-1996 (RFWS):
*        Added astWatch.
*     18-MAR-1998 (RFWS):
*        Make interface available for writing foreign language and
*        graphics interfaces, etc.
*     27-NOV-2002 (DSB):
*        Added astReporting.
*     14-MAR-2005 (NG):
*        Added astAssert
*     20-MAY-2005 (DSB):
*        Modified astAssert so that it does nothing if the AST error
*        status is already set, and also so that does nothing unless
*        the DEBUG macro is defined.
*     16-FEB-2006 (DSB):
*        Improve efficiency by replacing the astOK_ function with a macro
*        which tests the value of status variable. The pointer which points
*        to the status variable are now global rather than static.
*     1-MAR-2006 (DSB):
*        Remove astAssert.
*     19-SEP-2008 (DSB)
*        Big changes for thread-safe version of AST.
*-
*/

/* Suppress "operands are evaluated in unspecified order" warnings from
   the Intel icc compiler. These are caused by the astGetStatusPtr_
   function being called several times within each of the macros that
   form the public interface for AST. */
#ifdef __INTEL_COMPILER
#pragma warning(disable:981)
#endif


/* Include files. */
/* ============== */
#if defined(THREAD_SAFE)
#include <pthread.h>
#endif


/* Macros. */
/* ======= */
#if defined(astCLASS) || defined(astFORTRAN77) /* Protected */

/* Define a facility number that is unique to this library.  The number here
 * is the facility code assigned to the AST library, but it doesn't have to
 * be this number -- it only has to be probably unique.  If that code were
 * ever to change, then you can update this number if you feel it's tidier
 * that way. */
#define AST__FAC (1521)

/* Max number of messages which can be deferred when reporting is
   switched off. */
#define AST__ERROR_MSTACK_SIZE 100

#endif

/* This macro expands to an invocation of a specified function, together
   with a call to astAt to record the file and line number at which the
   invocation occurs. These are included in public error reports. This is
   only done for invocations from outside of AST (i.e. public invocations).*/
#if defined(astCLASS) || defined(astFORTRAN77)
#define astERROR_INVOKE(function) (function)
#else
#define astERROR_INVOKE(function) (astAt_(NULL,__FILE__,__LINE__,0,astGetStatusPtr),(function))
#endif

/* Define a dummy __attribute__ macro for use on non-GNU compilers. */
#ifndef __GNUC__
#  define  __attribute__(x)  /*NOTHING*/
#endif

/* Type definitions */
/* ================ */

/* Define a structure to hold information about an error context. */
typedef struct AstErrorContext {
   int reporting;   /* Value of error reporting flag at start of context */
   int ok;          /* Was the status value OK at start of context? */
   int status;      /* The status value at the start of the context */
} AstErrorContext;

#if defined(THREAD_SAFE) && ( defined(astCLASS) || defined(astFORTRAN77) )

/* Define a structure holding all data items that are global within the
   error.c file. */
typedef struct AstErrorGlobals {

/* Reporting flag: delivery of message is supressed if zero. */
   int Reporting;

/* Error context. */
   const char *Current_File;     /* Current file name pointer */
   const char *Current_Routine;  /* Current routine name pointer */
   int Current_Line;             /* Current line number */
   int Foreign_Set;              /* Have foreign values been set? */

/* Un-reported message stack */
   char *Message_Stack[ AST__ERROR_MSTACK_SIZE ];
   int Mstack_Size;

} AstErrorGlobals;

/* Structure to hold the internal status variable, and the status
   pointer for a single thread. */
typedef struct AstStatusBlock {
   int internal_status;
   int *status_ptr;
} AstStatusBlock;

#endif


/* Function Macros. */
/* ================ */

#if defined(astCLASS)

/*
*+
*  Name:
*     astErrorBegin

*  Purpose:
*     Begin a new error reporting context.

*  Type:
*     Protected macro.

*  Synopsis:
*     #include "error.h"
*     astErrorBegin( AstErrorContext *context );

*  Description:
*     This macro starts a new error reporting context. It saves any
*     existing error status in the supplied ontext structure, and then
*     clears the status value. It also defers further error reporting.
*
*     Each invocation of this macro should be followed (eventually) by a
*     matching invocation of astErrorEnd.

*  Parameters:
*     context
*        Pointer to a structure in which to to storeinformation about the
*        current error reporting context. This structure should be passed
*        unchanged to the corresponding invocation of astErrorEnd.

*-
*/
#define astErrorBegin(context) {\
\
/* Save the original error status value. */ \
   (context)->status = astStatus; \
\
/* Save a flag indicating if the original error status was good. */ \
   (context)->ok = astOK; \
\
/* Switch off the immediate delivery of error messages, recording the  \
   original value of the reporting flag. */ \
   (context)->reporting = astReporting( 0 ); \
\
/* Clear any existing error condition. */ \
   astClearStatus; \
}


/*
*+
*  Name:
*     astErrorEnd

*  Purpose:
*     End an error reporting context.

*  Type:
*     Protected macro.

*  Synopsis:
*     #include "error.h"
*     astErrorEnd( AstErrorContext *context );

*  Description:
*     This macro ends an error reporting context started using
*     astErrorBegin.
*
*     Each invocation of this macro should correspond to an earlier
*     invocation of astErrorBegin.

*  Parameters:
*     context
*        Pointer to a structure holding information returned by the
*        matching invocation of astErrorBegin.

*-
*/
#define astErrorEnd(context) { \
\
/* If an error condition existed when astErrorBegin was called, and \
   another error has since occurred, clear the deferred messages \
   reported during the error context without displaying them. */ \
   if( !(context)->ok && !astOK ) astClearStatus; \
\
/* Put the error reporting flag back to its original value. This will \
   have the effect of displaying any remaining errors reported within \
   the error context (they will already have been cleared if an error \
   condition existed at the start of the context). */ \
   astReporting( (context)->reporting ); \
\
/* If an error condition existed at the start of the context, re-instate \
   the original status value. */ \
   if( !(context)->ok ) astSetStatus( (context)->status ); \
}
#endif

/* Function prototypes. */
/* ==================== */

int *astWatch_( int * );
void astClearStatus_( int * );
int *astGetStatusPtr_( void )__attribute__((pure));
void astAt_( const char *, const char *, int, int, int * );

#if defined(astCLASS) || defined(astFORTRAN77)      /* Protected only */
int astReporting_( int, int * );
void astError_( int, const char *, int *, ... )__attribute__((format(printf,2,4)));
void astBacktrace_( int * );
#if defined(THREAD_SAFE)
void astInitErrorGlobals_( AstErrorGlobals * );
#endif
#endif

void astErrorPublic_( int, const char *, ... )__attribute__((format(printf,2,3)));



/* Function interfaces. */
/* ==================== */
/* These wrap up the functions defined by this module to make them
   easier to use. */

#define astWatch(status_ptr) astWatch_(status_ptr)
#define astGetStatusPtr astGetStatusPtr_()
#define astOK (astStatus==0)
#define astSetStatus(status_value) (astStatus=(status_value))

#if defined(astCLASS)     /* Protected */

#define astAt(routine,file,line) astAt_(routine,file,line,0,status)
#define astClearStatus astClearStatus_(status)
#define astStatus (*status)
#define astError astError_
#define astReporting(report) astReporting_(report,status)
#define astBacktrace astBacktrace_(status)

#elif defined(astFORTRAN77)

#define astAt(routine,file,line) astAt_(routine,file,line,1,STATUS)
#define astClearStatus astClearStatus_(status)
#define astStatus (*status)
#define astError astError_
#define astReporting(report) astReporting_(report,status)

#else

#define astAt(routine,file,line) astAt_(routine,file,line,1,astGetStatusPtr)
#define astClearStatus astClearStatus_(astGetStatusPtr)
#define astStatus (*astGetStatusPtr)
#define astError astErrorPublic_

#endif

#endif
