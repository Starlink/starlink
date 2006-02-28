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
*     DSB: David S Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-SEP-2005 (DSB):
*        Original version.
*     02-NOV-2005 (TIMJ):
*        Port from GRP
*     20-DEC-2005 (TIMJ):
*        Add ndgAsexp
*     28-FEB-2006 (DSB):
*        Use grpC2F and grpF2C.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*/

/* Header files. */
/* ============= */
#include "f77.h" 
#include "sae_par.h"
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

   IGRP = grpC2F( igrp, status );
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
  IGRP = grpC2F( igrp, status );
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

F77_SUBROUTINE(ndg_asexp)( CHARACTER(GRPEXP), LOGICAL(VERB), INTEGER(IGRP1), INTEGER(IGRP2),
			   INTEGER(SIZE), LOGICAL(FLAG), INTEGER(STATUS) TRAIL(GRPEXP) );

void ndgAsexp( char * grpexp, int verb, Grp *igrp1, Grp ** igrp2, int *size, int * flag, int *status ){
   DECLARE_INTEGER(IGRP1);
   DECLARE_INTEGER(IGRP2);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);
   DECLARE_LOGICAL(VERB);
   DECLARE_LOGICAL(FLAG);
   DECLARE_CHARACTER(GRPEXP, GRP__SZNAM);

   IGRP2 = grpC2F( *igrp2, status );
   IGRP1 = grpC2F( igrp1, status );
   if ( *status != SAI__OK ) return;

   F77_EXPORT_LOGICAL( verb, VERB );
   F77_EXPORT_CHARACTER( grpexp, GRPEXP, GRP__SZNAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(ndg_asexp)( CHARACTER_ARG(GRPEXP), LOGICAL_ARG(&VERB), INTEGER_ARG(&IGRP1),
                        INTEGER_ARG(&IGRP2),
                        INTEGER_ARG(&SIZE), LOGICAL_ARG(&FLAG),
                        INTEGER_ARG(&STATUS) TRAIL_ARG(GRPEXP) );

   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_LOGICAL( FLAG, *flag );
   
   F77_IMPORT_INTEGER( STATUS, *status );
   *igrp2 = (Grp *) grpF2C( IGRP2, status );

   return;
}
