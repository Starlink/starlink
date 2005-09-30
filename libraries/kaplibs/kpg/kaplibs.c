/*
*  Name:
*     kaplibs.c

*  Purpose:
*     Implement the C interface to the standalone routines in the KAPLIBS 
*     library.

*  Description:
*     This module implements C-callable wrappers for the public non-ADAM 
*     routines in the KAPLIBS library. The interface to these wrappers
*     is defined in kaplibs.h.

*  Notes:
*     - Given the size of the KAPLIBS library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use KAPLIBS from C extend this file (and
*     kaplibs.h) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David S Berry
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}
*/

/* Header files. */
/* ============= */
#include "f77.h"                 
#include "kaplibs.h"
#include "kaplibs_private.h"


/* Wrapper function implementations. */
/* ================================= */

void kpg1Kymap( int igrp, AstKeyMap **keymap, int *status ){
   kpg1Kymp1( igrp, keymap, status );
}




