/*
*  Name:
*     kaplibs_adam.c

*  Purpose:
*     Implement the C interface to the ADAM functions in KAPLIBS library.

*  Description:
*     This module implements C-callable wrappers for the public ADAM
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
#include "ast.h"                 
#include "f77.h"                 
#include "kaplibs.h"
#include "kaplibs_private.h"


/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(kpg1_asget)( INTEGER(INDF), INTEGER(NDIM), LOGICAL(EXACT), 
                            LOGICAL(TRIM), LOGICAL(REQINV), INTEGER_ARRAY(SDIM),
                            INTEGER_ARRAY(SLBND), INTEGER_ARRAY(SUBND),
                            INTEGER(IWCS), INTEGER(STATUS) );

void kpg1Asget( int indf, int ndim, int exact, int trim, int reqinv, 
                int *sdim, int *slbnd, int *subnd, AstFrameSet **iwcs, 
                int *status ){
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(NDIM);
   DECLARE_LOGICAL(EXACT);
   DECLARE_LOGICAL(TRIM);
   DECLARE_LOGICAL(REQINV);
   DECLARE_INTEGER_ARRAY_DYN(SDIM);
   DECLARE_INTEGER_ARRAY_DYN(SLBND);
   DECLARE_INTEGER_ARRAY_DYN(SUBND);
   DECLARE_INTEGER(IWCS);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( indf, INDF );
   F77_EXPORT_INTEGER( ndim, NDIM );
   F77_EXPORT_LOGICAL( exact, EXACT );
   F77_EXPORT_LOGICAL( trim, TRIM );
   F77_EXPORT_LOGICAL( reqinv, REQINV );
   F77_CREATE_INTEGER_ARRAY( SDIM, ndim );
   F77_ASSOC_INTEGER_ARRAY( SDIM, sdim );
   F77_CREATE_INTEGER_ARRAY( SLBND, ndim );
   F77_ASSOC_INTEGER_ARRAY( SLBND, slbnd );
   F77_CREATE_INTEGER_ARRAY( SUBND, ndim );
   F77_ASSOC_INTEGER_ARRAY( SUBND, subnd );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(kpg1_asget)( INTEGER_ARG(&INDF),
                         INTEGER_ARG(&NDIM),
                         LOGICAL_ARG(&EXACT),
                         LOGICAL_ARG(&TRIM),
                         LOGICAL_ARG(&REQINV),
                         INTEGER_ARRAY_ARG(SDIM),
                         INTEGER_ARRAY_ARG(SLBND),
                         INTEGER_ARRAY_ARG(SUBND),
                         INTEGER_ARG(&IWCS),
                         INTEGER_ARG(&STATUS) );

   {
      int tmp;
      F77_IMPORT_INTEGER( IWCS, tmp );
      *iwcs = astI2P( tmp );
   }

   F77_IMPORT_INTEGER_ARRAY( SDIM, sdim, ndim );
   F77_FREE_INTEGER( SDIM );

   F77_IMPORT_INTEGER_ARRAY( SLBND, slbnd, ndim );
   F77_FREE_INTEGER( SLBND );

   F77_IMPORT_INTEGER_ARRAY( SUBND, subnd, ndim );
   F77_FREE_INTEGER( SUBND );

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


