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
*     #include "err.h"

*  Description:
*     This include file defines the interface to the err module and
*     provides the type definitions, function prototypes and macros, etc.
*     needed to use this module.

*  Inheritance:
*     The err module is not a class and does not inherit.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-1996 (DSB):
*        Original version.
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
