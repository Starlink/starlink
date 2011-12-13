/*
*  Name:
*     err_null.c

*  Purpose:
*     Implement the default "err" module.

*  Description:
*     This file implements the default "err" module for the AST
*     library. It is used to deliver error messages if no alternative
*     error delivery mechanism is provided.
*
*     To provide an alternative mechanism, re-implement this module
*     and link your program against the resulting library.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-1996 (DSB):
*        Original version.
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
#include "err.h"                 /* Interface to this module */
#include "error.h"               /* Interface to the error module */

/* C header files. */
/* --------------- */
#include <stdio.h>

/* Function implementations. */
/* ========================= */
void astPutErr_( int status_value, const char *message ) {
/*
*+
*  Name:
*     astPutErr

*  Purpose:
*     Deliver an error message.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "err.h"
*     void astPutErr( int status_value, const char *message )

*  Description:
*     This function delivers an error message and (optionally) an
*     accompanying status value to the user. It may be re-implemented
*     in order to deliver error messages in different ways, according
*     to the environment in which the AST library is being used.

*  Parameters:
*     status_value
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


/* This is the default implementation. Simply write the message to
   standard error with a newline appended. Ignore the status value. */
   int *status = astGetStatusPtr;
   (void) fprintf( stderr, "%s%s\n", astOK ? "!! " : "!  ", message );
}
