/*
*  Name:
*     grp.c

*  Purpose:
*     Implement the C interface to the GRP library.

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the GRP library. The interface to these wrappers
*     is defined in grp.h.

*  Notes:
*     - Given the size of the GRP library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use GRP from C extend this file (and
*     grp.h) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David S Berry
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}
*/

/* Header files. */
/* ============= */
#include "ast.h"                 
#include "f77.h"                 
#include "grp.h"

/* Wrapper function implementations. */
/* ================================= */


F77_SUBROUTINE(grp_grpsz)( INTEGER(IGRP), INTEGER(SIZE), INTEGER(STATUS) );

void grpGrpsz( int igrp, int *size, int *status ){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( igrp, IGRP );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(grp_grpsz)( INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&SIZE),
                        INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER( SIZE, size );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


