#if !defined( ERR_INCLUDED ) /* Include this file only once */
#define ERR_INCLUDED
/*
 *+
 *  Name:
 *     err.h

 *  Type:
 *     C include file.

 *  Purpose:
 *     Define the interface to the err module.
 
 *  Invocation:
 *     #include "asttcl_err.h"
 
 *  Description:
 *     This include file defines the interface to the err module and
 *     provides the type definitions, function prototypes and macros, etc.
 *     needed to use this module.
 
 *  Inheritance:
 *     The err module is not a class and does not inherit.

 *  Copyright:
 *     Copyright (C) 1998 Central Laboratory of the Research Councils
 
 *  Authors:
 *     RFWS: R.F. Warren-Smith (STARLINK)
 *     DSB: David Berry (STARLINK)
 *     PWD: Peter W. Draper (STARLINK)
 *     {enter_new_authors_here}
 
 *  History:
 *     6-NOV-1996 (DSB):
 *        Original version.
 *     8-SEP-1997 (PWD):
 *        Converted for use with Tcl.
 *     {enter_changes_here}
 *-
 */

/* Function prototypes. */
/* ==================== */
#if defined(astCLASS)            /* Protected */
void astPutErr_( int, const char * );

/* Function interfaces. */
/* ==================== */
#define astPutErr(status,message) astPutErr_(status,message)
#endif

#endif
