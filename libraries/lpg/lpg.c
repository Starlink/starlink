/*
*+
*  Name:
*     lpg.c

*  Purpose:
*     Implement the C interface to the LPG routines.

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the LPG library. The interface to these wrappers
*     is defined in lpg.h.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     22-SEP-2020 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*/

/* Header files. */
/* ============= */
#include "f77.h"
#include "sae_par.h"
#include "mers.h"
#include "star/mem.h"
#include "star/hds_fortran.h"
#include "lpg.h"
#include <string.h>

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(lpg_assoc)( CHARACTER(PARAM),
                           CHARACTER(MODE),
                           INTEGER(INDF),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(MODE) );


void lpgAssoc( const char *param, const char *mode, int *indf, int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_CHARACTER_DYN(MODE);
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_CREATE_EXPORT_CHARACTER( mode, MODE );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(lpg_assoc)( CHARACTER_ARG(PARAM),
                                  CHARACTER_ARG(MODE),
                                  INTEGER_ARG(&INDF),
                                  INTEGER_ARG(&STATUS)
                                  TRAIL_ARG(PARAM)
                                  TRAIL_ARG(MODE)); )

   F77_FREE_CHARACTER( PARAM );
   F77_FREE_CHARACTER( MODE );

   F77_IMPORT_INTEGER( INDF, *indf );
   F77_IMPORT_INTEGER( STATUS, *status );
}

/* ------------------------------- */

F77_SUBROUTINE(lpg_prop)( INTEGER(INDF1),
                          CHARACTER(CLIST),
                          CHARACTER(PARAM),
                          INTEGER(INDF2),
                          INTEGER(STATUS)
                          TRAIL(CLIST)
                          TRAIL(PARAM) );

void lpgProp( int indf1, const char *clist, const char *param, int *indf2, int *status ){
   DECLARE_INTEGER(INDF1);
   DECLARE_CHARACTER_DYN(CLIST);
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(INDF2);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( indf1, INDF1 );
   F77_CREATE_EXPORT_CHARACTER( clist, CLIST );
   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(lpg_prop)( INTEGER_ARG(&INDF1),
                                 CHARACTER_ARG(CLIST),
                                 CHARACTER_ARG(PARAM),
                                 INTEGER_ARG(&INDF2),
                                 INTEGER_ARG(&STATUS)
                                 TRAIL_ARG(CLIST)
                                 TRAIL_ARG(PARAM)); )

   F77_FREE_CHARACTER( CLIST );
   F77_FREE_CHARACTER( PARAM );

   F77_IMPORT_INTEGER( INDF2, *indf2 );
   F77_IMPORT_INTEGER( STATUS, *status );
}

/* ------------------------------- */

F77_SUBROUTINE(lpg_state)( CHARACTER(PARAM),
                           INTEGER(STATE),
                           INTEGER(STATUS)
                           TRAIL(PARAM) );

void lpgState( const char *param, int *state, int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(STATE);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(lpg_state)( CHARACTER_ARG(PARAM),
                                  INTEGER_ARG(&STATE),
                                  INTEGER_ARG(&STATUS)
                                  TRAIL_ARG(PARAM)); )

   F77_FREE_CHARACTER( PARAM );
   F77_IMPORT_INTEGER( STATE, *state );
   F77_IMPORT_INTEGER( STATUS, *status );
}

