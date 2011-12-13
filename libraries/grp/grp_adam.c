/*
*  Name:
*     grp_adam.c

*  Purpose:
*     Implement the C interface for the ADAM routines in the GRP library.

*  Description:
*     This module implements C-callable wrappers for the public
*     ADAM routines in the GRP library. The interface to these wrappers
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
*     4-APR-2008 (DSB):
*        Original version.
*     15-JUL-2008 (TIMJ):
*        Use size_t for index argument.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*/

/* Header files. */
/* ============= */
#include "f77.h"
#include "grp1.h"
#include "grp_err.h"
#include "sae_par.h"
#include "merswrap.h"
#include "star/mem.h"
#include <string.h>

/* These are defined in grpc. */
extern int grpSlot( int, int * );
extern Grp *Grp_Pointers[ GRP__MAXG ];

/* Prototypes for local static functions */
/* ===================================== */

F77_SUBROUTINE(grp_list)( CHARACTER(PARAM),
                          INTEGER(INDXLO),
                          INTEGER(INDXHI),
                          CHARACTER(COMNT),
                          INTEGER(IGRP),
                          INTEGER(STATUS)
                          TRAIL(PARAM)
                          TRAIL(COMMNT) );

void grpList( const char *param, size_t indxlo, size_t indxhi,
               const char *comnt, Grp *grp, int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(INDXLO);
   DECLARE_INTEGER(INDXHI);
   DECLARE_CHARACTER_DYN(COMNT);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_CHARACTER( PARAM, strlen( param ) );
   F77_EXPORT_CHARACTER( param, PARAM, PARAM_length );

   F77_EXPORT_INTEGER( indxlo, INDXLO );
   F77_EXPORT_INTEGER( indxhi, INDXHI );

   F77_CREATE_CHARACTER( COMNT, comnt ? strlen( comnt ) : 1 );
   F77_EXPORT_CHARACTER( comnt ? comnt : " ", COMNT, COMNT_length );

   IGRP = grpC2F( grp, status );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(grp_list)( CHARACTER_ARG(PARAM),
                       INTEGER_ARG(&INDXLO),
                       INTEGER_ARG(&INDXHI),
                       CHARACTER_ARG(COMNT),
                       INTEGER_ARG(&IGRP),
                       INTEGER_ARG(&STATUS)
                       TRAIL_ARG(PARAM)
                       TRAIL_ARG(COMNT) ); )

   F77_FREE_CHARACTER( PARAM );
   F77_FREE_CHARACTER( COMNT );

   F77_IMPORT_INTEGER( STATUS, *status );
}


