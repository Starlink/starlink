/*
 *  Name:
 *     ast_tclerr.c
 *
 *  Purpose:
 *     Implements error reporting for Tcl applications.
 *
 *  Description:
 *     This routine implements the AST error reporting mechanism to
 *     work though Tcl. Before this can be used a call to the routine
 *     errTcl_Init must be made. This establishes the name of the Tcl
 *     interpretor that should deal with report.
 *
 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 *
 *  Authors:
 *     PWD: Peter W. Draper (Durham University- STARLINK)
 *     {enter_new_authors_here}
 *
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
/* Interface definitions. */
/* ---------------------- */
#include "ast.h"                 /* AST interface */
#include "tcl.h"
#include "ast_tclerr.h"          /* Interface to this module */

/* C header files. */
/* --------------- */
#include <stdio.h>

/* Static variables. */
/* ----------------- */
static Tcl_Interp *Interp = NULL;  /* Tcl interpreter for any messages*/
static int Report = 1;             /* Whether to report errors */

/* Function implementations. */
/* ========================= */

void errTcl_Init( Tcl_Interp *newinterp) {
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
  Interp = newinterp;
}

int errTcl_Inhibit( int report ) {
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
  int oldvalue = Report;
  Report = report;
  return oldvalue;
}


void astPutErr_( int status, const char *message ) {
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
  
  /*  If available then return the message through the Tcl
      interpreter, otherwise just act like the null implementation of
      this routine. */
  if ( Interp != NULL ) {
    Tcl_AppendResult( Interp, (char *) message, "\n", (char *)NULL );
#ifdef DEBUG 
    (void) fprintf( stderr, "DEBUG: %s%s\n", astOK ? "!! " : "!  ", message);
#endif
  } else {
    (void) fprintf( stderr, "%s%s\n", astOK ? "!! " : "!  ", message );
  }
#ifdef DEBUG 
  (void) fflush(stderr);
#endif
}
