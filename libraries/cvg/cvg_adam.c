/*
*  Name:
*     cvg_adam.c

*  Purpose:
*     Implement the C interface to the ADAM cvg library.

*  Description:
*     This module implements C-callable wrappers for the public ADAM
*     routines in the cvg library. The interface to these wrappers
*     is defined in cvg.h.

*  Authors:
*     DSB: David S Berry
*     {enter_new_authors_here}

*  History:
*     14-NOV-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
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
#include "sae_par.h"
#include "star/ndg.h"
#include "cvg.h"
#include "ast.h"

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(cvg_scadc)( INTEGER(IPROV),
                           CHARACTER(PARAM),
                           INTEGER(STATUS)
                           TRAIL(PARAM) );

void cvgScadc( NdgProvenance *prov, const char *param, int *status ){
   DECLARE_INTEGER(IPROV);
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( prov ), IPROV );
   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(cvg_scadc)( INTEGER_ARG(&IPROV),
                                  CHARACTER_ARG(PARAM),
                                  INTEGER_ARG(&STATUS)
                                  TRAIL_ARG(PARAM) ); )

   F77_FREE_CHARACTER( PARAM );
   F77_IMPORT_INTEGER( STATUS, *status );
}







F77_SUBROUTINE(cvg_creat)( CHARACTER(PARAM),
                           INTEGER(BLOCKF),
                           LOGICAL(OVRWRT),
                           INTEGER(FUNIT),
                           INTEGER(STATUS)
                           TRAIL(PARAM) );

void cvgCreat( const char *param, int blockf, int ovrwrt, fitsfile **fptr,
               int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(BLOCKF);
   DECLARE_LOGICAL(OVRWRT);
   DECLARE_INTEGER(FUNIT);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_EXPORT_INTEGER( blockf, BLOCKF );
   F77_EXPORT_LOGICAL( ovrwrt, OVRWRT );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(cvg_creat)( CHARACTER_ARG(PARAM),
                                  INTEGER_ARG(&BLOCKF),
                                  LOGICAL_ARG(&OVRWRT),
                                  INTEGER_ARG(&FUNIT),
                                  INTEGER_ARG(&STATUS)
                                  TRAIL_ARG(PARAM) ); )

   F77_FREE_CHARACTER( PARAM );
   CVG_IMPORT_FITS( FUNIT, *fptr );
   F77_IMPORT_INTEGER( STATUS, *status );
}





F77_SUBROUTINE(cvg_assoc)( CHARACTER(PARAM),
                           CHARACTER(MODE),
                           INTEGER(FUNIT),
                           INTEGER(BLOCKF),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(MODE) );

void cvgAssoc( const char *param, const char *mode, fitsfile **fptr,
               int *blockf, int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_CHARACTER_DYN(MODE);
   DECLARE_INTEGER(FUNIT);
   DECLARE_INTEGER(BLOCKF);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_CREATE_EXPORT_CHARACTER( mode, MODE );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(cvg_assoc)( CHARACTER_ARG(PARAM),
                                  CHARACTER_ARG(MODE),
                                  INTEGER_ARG(&FUNIT),
                                  INTEGER_ARG(&BLOCKF),
                                  INTEGER_ARG(&STATUS)
                                  TRAIL_ARG(PARAM)
                                  TRAIL_ARG(MODE) ); )

   F77_FREE_CHARACTER( PARAM );
   F77_FREE_CHARACTER( MODE );
   F77_IMPORT_INTEGER( BLOCKF, *blockf );
   CVG_IMPORT_FITS( FUNIT, *fptr );
   F77_IMPORT_INTEGER( STATUS, *status );
}

