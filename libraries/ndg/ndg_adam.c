/*
*  Name:
*     ndg.c

*  Purpose:
*     Implement the C interface to the ADAM NDG library.

*  Description:
*     This module implements C-callable wrappers for the public ADAM
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
*     28-FEB-2006 (DSB):
*        Use grpF2C and grpC2F.
*     11-JUL-2006 (TIMJ):
*        Add some const-ing
*     4-JUL-2008 (TIMJ):
*        More const-ing goodness.
*     15-JUL-2008 (TIMJ):
*        Use size_t for index to match new Grp interface.
*     6-MAY-2010 (DSB):
*        Move ndgAddgh from ndg.c to ndg_adam.c
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
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


F77_SUBROUTINE(ndg_assoc)( CHARACTER(PARAM), LOGICAL(VERB), INTEGER(IGRP), INTEGER(SIZE), LOGICAL(FLAG), INTEGER(STATUS) TRAIL(PARAM) );

void ndgAssoc( const char * param, int verb, Grp ** igrp, size_t *size, int * flag, int *status
){
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);
   DECLARE_LOGICAL(VERB);
   DECLARE_LOGICAL(FLAG);
   DECLARE_CHARACTER(PARAM, PAR__SZNAM);

   IGRP = grpC2F(*igrp, status );
   if ( *status != SAI__OK ) return;

   F77_EXPORT_LOGICAL( verb, VERB );
   F77_EXPORT_CHARACTER( param, PARAM, PAR__SZNAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_assoc)( CHARACTER_ARG(PARAM), LOGICAL_ARG(&VERB),
			INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&SIZE), LOGICAL_ARG(&FLAG),
                        INTEGER_ARG(&STATUS) TRAIL_ARG(PARAM) ); )

   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_LOGICAL( FLAG, *flag );
   F77_IMPORT_INTEGER( STATUS, *status );
   *igrp = (Grp *) grpF2C( IGRP, status );

   return;
}


F77_SUBROUTINE(ndg_creat)( CHARACTER(PARAM), INTEGER(IGRP0), INTEGER(IGRP), INTEGER(SIZE), LOGICAL(FLAG), INTEGER(STATUS) TRAIL(PARAM) );

void ndgCreat( const char * param, const Grp *igrp0, Grp ** igrp, size_t *size, int * flag, int *status
){
   DECLARE_INTEGER(IGRP0);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);
   DECLARE_LOGICAL(FLAG);
   DECLARE_CHARACTER(PARAM, PAR__SZNAM);

   IGRP = grpC2F( *igrp, status );
   IGRP0 = grpC2F( igrp0, status );
   if ( *status != SAI__OK ) return;

   F77_EXPORT_CHARACTER( param, PARAM, PAR__SZNAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(ndg_creat)( CHARACTER_ARG(PARAM), INTEGER_ARG(&IGRP0),
			INTEGER_ARG(&IGRP),
                        INTEGER_ARG(&SIZE), LOGICAL_ARG(&FLAG),
                        INTEGER_ARG(&STATUS) TRAIL_ARG(PARAM) ); )

   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_LOGICAL( FLAG, *flag );

   F77_IMPORT_INTEGER( STATUS, *status );
   *igrp = (Grp *) grpF2C( IGRP, status );

   return;
}

F77_SUBROUTINE(ndg_addgh)( CHARACTER(PARAM), INTEGER(IGRP), INTEGER(STATUS)
                           TRAIL(PARAM) );

void ndgAddgh( const char param[], const Grp * igrp, int * status ) {
  DECLARE_INTEGER(IGRP);
  DECLARE_CHARACTER( PARAM, PAR__SZNAM );
  DECLARE_INTEGER(STATUS);

  IGRP = grpC2F( igrp, status );
  if (*status != SAI__OK) return;

  F77_EXPORT_CHARACTER( param, PARAM, PAR__SZNAM );
  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(ndg_addgh)( CHARACTER_ARG(PARAM), INTEGER_ARG(&IGRP),
                       INTEGER_ARG(&STATUS) TRAIL_ARG(PARAM) ); )

  F77_IMPORT_INTEGER( STATUS, *status );
  return;
}

