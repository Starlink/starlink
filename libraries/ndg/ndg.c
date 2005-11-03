/*
*  Name:
*     ndg.c

*  Purpose:
*     Implement the C interface to the NDG library.

*  Description:
*     This module implements C-callable wrappers for the public non-ADAM
*     routines in the NDG library. The interface to these wrappers
*     is defined in ndg.h.

*  Notes:
*     - Given the size of the NDG library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that 
*     people who want to use NDG from C extend this file (and
*     ndg.h) to include any functions which they need but which are
*     not already included.

*  Authors:
*     DSB: David S Berry
*     TIMJ: Tim Jenness
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     02-NOV-2005 (TIMJ):
*        Port from GRP
*     {enter_further_changes_here}
*/

/* Header files. */
/* ============= */
#include "f77.h" 
#include "par_par.h"
#include "star/grp.h"
#include "ndg.h"

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(ndg_ndfas)( INTEGER(IGRP), INTEGER(INDEX), CHARACTER(MODE), INTEGER(INDF), 
INTEGER(STATUS) TRAIL(MODE) );

void ndgNdfas( Grp *igrp, int index, char * mode, int * indf, int * status ) {
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(INDEX);
   DECLARE_CHARACTER(MODE, 10);
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(STATUS);

   IGRP = grp1Getid( igrp, status );
   F77_EXPORT_INTEGER(index, INDEX );
   F77_EXPORT_CHARACTER( mode, MODE, 10 );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(ndg_ndfas)( INTEGER_ARG(&IGRP), INTEGER_ARG(&INDEX),
                        CHARACTER_ARG(MODE), INTEGER_ARG(&INDF),
                        INTEGER_ARG(&STATUS) TRAIL_ARG(MODE) );

   F77_IMPORT_INTEGER( INDF, *indf );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


F77_SUBROUTINE(ndg_ndfpr)( INTEGER(INDF1), CHARACTER(CLIST), INTEGER(IGRP), INTEGER(INDEX), INTEGER(INDF2), INTEGER(STATUS) TRAIL(CLIST));

void ndgNdfpr( int indf1, char * clist, Grp *igrp, int index, int * indf2, int * status) {

  DECLARE_INTEGER(INDF1);
  DECLARE_CHARACTER(CLIST, 128);
  DECLARE_INTEGER(IGRP);
  DECLARE_INTEGER(INDEX);
  DECLARE_INTEGER(INDF2);
  DECLARE_INTEGER(STATUS);

  F77_EXPORT_INTEGER(indf1, INDF1);
  IGRP = grp1Getid( igrp, status );
  F77_EXPORT_CHARACTER( clist, CLIST, 128);
  F77_EXPORT_INTEGER(index, INDEX);
  F77_EXPORT_INTEGER(*status, STATUS);

  F77_CALL(ndg_ndfpr)( INTEGER_ARG(&INDF1), CHARACTER_ARG(CLIST),
		       INTEGER_ARG(&IGRP), INTEGER_ARG(&INDEX),
		       INTEGER_ARG(&INDF2), INTEGER_ARG(&STATUS)
		       TRAIL_ARG(CLIST) );

  F77_IMPORT_INTEGER( STATUS, *status);
  F77_IMPORT_INTEGER( INDF2, *indf2);

}

