/*
*  Name:
*     cvg.c

*  Purpose:
*     Implement the C interface to the CVG library.

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the CVG library. The interface to these wrappers
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
#include "cvg.h"

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(cvg_close)( INTEGER(FUNIT),
                           INTEGER(STATUS) );

void cvgClose( int *funit, int *status ){
   DECLARE_INTEGER(FUNIT);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( *funit, FUNIT );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(cvg_close)( INTEGER_ARG(&FUNIT),
                        INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER( FUNIT, *funit );

   return;
}

F77_SUBROUTINE(cvg_new)( CHARACTER(PATH),
                         INTEGER(BLOCKF),
                         LOGICAL(OVRWRT),
                         INTEGER(FUNIT),
                         INTEGER(STATUS)
                         TRAIL(PARAM) );

void cvgNew( const char *path, int blockf, int ovrwrt, int *funit,
             int *status ){
   DECLARE_CHARACTER_DYN(PATH);
   DECLARE_INTEGER(BLOCKF);
   DECLARE_LOGICAL(OVRWRT);
   DECLARE_INTEGER(FUNIT);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( path, PATH );
   F77_EXPORT_INTEGER( blockf, BLOCKF );
   F77_EXPORT_LOGICAL( ovrwrt, OVRWRT );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(cvg_new)( CHARACTER_ARG(PATH),
                                INTEGER_ARG(&BLOCKF),
                                LOGICAL_ARG(&OVRWRT),
                                INTEGER_ARG(&FUNIT),
                                INTEGER_ARG(&STATUS)
                                TRAIL_ARG(PATH) ); )

   F77_FREE_CHARACTER( PATH );
   F77_IMPORT_INTEGER( FUNIT, *funit );
   F77_IMPORT_INTEGER( STATUS, *status );
}

F77_SUBROUTINE(cvg_ft2bt)( INTEGER(TABLE),
                           INTEGER(FUNIT),
                           CHARACTER(EXTNAM),
                           INTEGER(ASTVER),
                           LOGICAL(MKCHDU),
                           INTEGER(STATUS)
                           TRAIL(EXTNAM) );

void cvgFt2bt( AstFitsTable *table, int funit, const char *extnam,
               int astver, int mkchdu, int *status ){
   DECLARE_INTEGER(TABLE);
   DECLARE_INTEGER(FUNIT);
   DECLARE_CHARACTER_DYN(EXTNAM);
   DECLARE_INTEGER(ASTVER);
   DECLARE_LOGICAL(MKCHDU);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( table ), TABLE );

   if( !astOK ) return;

   F77_EXPORT_INTEGER( funit, FUNIT );
   F77_CREATE_CHARACTER( EXTNAM, strlen( extnam ) );
   F77_EXPORT_CHARACTER( extnam, EXTNAM, EXTNAM_length );
   F77_EXPORT_INTEGER( astver, ASTVER );
   F77_EXPORT_LOGICAL( mkchdu, MKCHDU );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(cvg_ft2bt)( INTEGER_ARG(&TABLE),
                                  INTEGER_ARG(&FUNIT),
                                  CHARACTER_ARG(EXTNAM),
                                  INTEGER_ARG(&ASTVER),
                                  LOGICAL_ARG(&MKCHDU),
                                  INTEGER_ARG(&STATUS)
                                  TRAIL_ARG(EXTNAM) ); )

   F77_FREE_CHARACTER( EXTNAM );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


F77_SUBROUTINE(cvg_showheader)( INTEGER(FUNIT),
                                LOGICAL(ALL),
                                INTEGER(STATUS) );

void cvgShowHeader( int funit, int all, int *status ){
   DECLARE_INTEGER(FUNIT);
   DECLARE_LOGICAL(ALL);
   DECLARE_INTEGER(STATUS);
   F77_EXPORT_INTEGER( funit, FUNIT );
   F77_EXPORT_LOGICAL( all, ALL );
   F77_EXPORT_INTEGER( *status, STATUS );
   F77_LOCK( F77_CALL(cvg_showheader)( INTEGER_ARG(&FUNIT),
                                       LOGICAL_ARG(&ALL),
                                       INTEGER_ARG(&STATUS) ); )
   F77_IMPORT_INTEGER( STATUS, *status );
   return;
}


F77_SUBROUTINE(cvg_pcadc)( INTEGER(IPROV),
                           INTEGER(FUNIT),
                           INTEGER(STATUS) );

void cvgPcadc( NdgProvenance *prov, int funit, int *status ){
   DECLARE_INTEGER(IPROV);
   DECLARE_INTEGER(FUNIT);
   DECLARE_INTEGER(STATUS);
   F77_EXPORT_INTEGER( funit, FUNIT );
   F77_EXPORT_INTEGER( *status, STATUS );
   F77_EXPORT_INTEGER( astP2I( prov ), IPROV );
   if( !astOK ) return;
   F77_LOCK( F77_CALL(cvg_pcadc)( INTEGER_ARG(&IPROV),
                                  INTEGER_ARG(&FUNIT),
                                  INTEGER_ARG(&STATUS) ); )
   F77_IMPORT_INTEGER( STATUS, *status );
   return;
}
