/*
 *  Name:
 *     ast_tclerr.c

 *  Purpose:
 *     Implements error reporting for Tcl applications.

 *  Description:
 *     This routine implements the AST error reporting mechanism to
 *     work though Tcl. Before this can be used a call to the routine
 *     errTcl_Init must be made. This establishes the name of the Tcl
 *     interpreter that should deal with the report.

 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *
 *  Authors:
 *     PWD: Peter W. Draper (Durham University- STARLINK)
 *     {enter_new_authors_here}

 *  History:
 *     8-SEP-1997 (PWD):
 *        Original version. Based on err_null.c module.
 *     {enter_changes_here}
 */

/* Module Macros. */
/* ============== */
/* Define the astCLASS macro (even although this is not a class
   implementation). This indicates to header files that they should
   make "protected" symbols available. */
#define astCLASS

/* Include files. */
/* ============== */
#if HAVE_CONFIG_H
#include "config.h"
#endif

/* Interface definitions. */
/* ---------------------- */
#include "ast.h"                 /* AST interface */
#include "tcl.h"
#include "ast_tclerr.h"          /* Interface to this module */
#include "sae_par.h"

/* C header files. */
/* --------------- */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* Static variables. */
/* ----------------- */
static Tcl_Interp *Interp = NULL;    /* Tcl interpreter for any messages */
static int Report = 1;               /* Whether to report errors, currently
                                      * ignored  */
static const char *lastError = NULL; /* The last error message */
static int lastStatus = SAI__OK;     /* The last error status */

/* Function implementations. */
/* ========================= */

/*
 *+
 *  Name:
 *     errTcl_Init

 *  Purpose:
 *     Initialise error reporting system for Tcl.

 *  Synopsis:
 *     #include "tcl_err.h"
 *     void errTcl_Init( Tcl_interp *newinterp )

 *  Description:
 *     This function initialises the AST error reporting system to
 *     return any error message via the Tcl interpreter.

 *  Parameters:
 *     newinterp
 *        The Tcl interpreter that is being used and which should have
 *        any error message returned in its result.

 *-
 */
void errTcl_Init( Tcl_Interp *newinterp)
{
    Interp = newinterp;
}

/*
 *+
 *  Name:
 *     errTcl_Inhibit

 *  Purpose:
 *     Controls whether error reports are actually made.

 *  Synopsis:
 *     #include "tcl_err.h"
 *     int errTcl_Init( int report )

 *  Description:
 *     This function allows the suppression of error messages. It also
 *     returns the current status of error reporting.

 *  Parameters:
 *     int report
 *        If 1 then error messages will be reported. Otherwise they
 *        will be inhibitied, until this routine is called to enable
 *        them.

 *-
 */
int errTcl_Inhibit( int report )
{
    int oldvalue = Report;
    Report = report;
    return oldvalue;
}

/*
 *+
 *  Name:
 *     errTcl_LastError

 *  Purpose:
 *     Returns the last error message and status.

 *  Synopsis:
 *     #include "tcl_err.h"
 *     void errTcl_LastError( int *status, const char **message )

 *  Description:
 *     This function returns the last error status seen and the associated
 *     message. The message is volatile and should be copied if kept.

 *  Parameters:
 *     int *status
 *        The status value;
 *     const char **message
 *        The message.

 *-
 */
void errTcl_LastError( int *status, const char **message )
{
    *status = lastStatus;
    *message = lastError;
}

/*
 *+
 *  Name:
 *     astPutErr

 *  Purpose:
 *     Deliver an error message.

 *  Type:
 *     Protected function.

 *  Synopsis:
 *     #include "asttcl_err.h"
 *     void astPutErr( int status, const char *message )

 *  Description:
 *     This function delivers an error message and (optionally) an
 *     accompanying status value to the user. It may be re-implemented
 *     in order to deliver error messages in different ways, according
 *     to the environment in which the AST library is being used.

 *  Parameters:
 *     status
 *        The error status value.
 *     message
 *        A pointer to a null-terminated character string containing
 *        the error message to be delivered. This should not contain
 *        newline characters.

 *  Notes:
 *     - This function is documented as "protected" but, in fact, is
 *     publicly accessible so that it may be re-implemented as
 *     required.
 *-
 */
void astPutErr_( int status, const char *message )
{

    /* By default we pass any AST related to FITS back through the Tcl
     * interpreter and through standard error. These are important and
     * resolve many issues with bad WCS calibrations, other errors are
     * only delivered through standard error if DEBUG is set. If no
     * interpreter is available then all messages are delivered through
     * standard error.
     */
    if ( Interp != NULL ) {
        Tcl_AppendResult( Interp, (char *) message, "\n", (char *)NULL );

#ifdef DEBUG
        (void) fprintf( stderr, "DEBUG: %s%s\n", astOK ? "!! " : "!  ",
                        message );
        (void) fflush( stderr );
#else
        if ( strstr( message, "FITS" ) != NULL ||
             strstr( message, "Fits" ) != NULL ) {
            (void) fprintf( stderr, "%s%s\n", astOK ? "!! " : "!  ", message );
            (void) fflush( stderr );
        }
#endif
    } else {
        (void) fprintf( stderr, "%s%s\n", astOK ? "!! " : "!  ", message );
        (void) fflush( stderr );
    }

    /* Keep references to the status and message */
    if ( lastError != NULL ) {
        free( (void *) lastError );
    }
    lastError = strdup( message );
    lastStatus = status;
}
