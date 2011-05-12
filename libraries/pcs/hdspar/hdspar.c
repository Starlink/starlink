#define _POSIX_SOURCE 1		 /* Declare POSIX source */
/*
*+
*  Name:
*     hdspar.c

*  Purpose:
*     Implement the public C interface to the HDSPAR library.

*  Language:
*     ANSI C

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the HDSPAR library.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     31-JAN-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}
*-
*/

/* Header files. */
/* ============= */
/* C run-time library header files. */
#include <ctype.h>               /* Character class tests */
#include <stdlib.h>              /* Utility functions */
#include <string.h>              /* String handling */

/* External interface header files. */
#include "dat_par.h"             /* Hierarchical Data System (HDS) */
#include "f77.h"                 /* C<-->Fortran interface macros */
#include "sae_par.h"             /* SAI__OK */
#include "ems.h"                 /* ems prototypes */

/* HDS Fortran Locator export/import routines */
#include "star/hds_fortran.h"

/* Internal header files. */
#include "hdspar.h"                 /* HDSPAR_ library public interface */

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(dat_assoc)( CHARACTER(PARAM),
                           CHARACTER(ACCESS),
                           CHARACTER(LOC),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(ACCESS)
                           TRAIL(LOC) );

void datAssoc( const char *param,
               const char *access,
               HDSLoc **loc,
               int *status ) {

DECLARE_CHARACTER_DYN(PARAM);
DECLARE_CHARACTER_DYN(ACCESS);
DECLARE_CHARACTER(LOC,DAT__SZLOC);
DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_CREATE_EXPORT_CHARACTER( access, ACCESS );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(dat_assoc)( CHARACTER_ARG(PARAM),
                        CHARACTER_ARG(ACCESS),
                        CHARACTER_ARG(LOC),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(PARAM)
                        TRAIL_ARG(ACCESS)
                        TRAIL_ARG(LOC) ); )

   F77_FREE_CHARACTER( PARAM );
   F77_FREE_CHARACTER( ACCESS );
   F77_IMPORT_INTEGER( STATUS, *status );

   if( *status == SAI__OK ) {
      HDS_IMPORT_FLOCATOR( LOC, loc, status );
      if( *status != SAI__OK ) {
         *loc = NULL;
         emsRep( "datAssoc_err", "datAssoc: Error obtaining HDS locator "
                 "from an environment parameter.", status );
      }
   }

   return;
}


F77_SUBROUTINE(dat_cancl)( CHARACTER(PARAM),
                           INTEGER(STATUS)
                           TRAIL(PARAM) );

void datCancl( const char *param, int *status ) {

DECLARE_CHARACTER_DYN(PARAM);
DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(dat_cancl)( CHARACTER_ARG(PARAM),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(PARAM) ); )

   F77_FREE_CHARACTER( PARAM );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


F77_SUBROUTINE(dat_creat)( CHARACTER(PARAM),
                           CHARACTER(TYPE),
                           INTEGER(NDIMS),
                           INTEGER_ARRAY(DIMS),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(TYPE) );

void datCreat( const char *param,
               const char *type,
               int ndims,
               const int dims[],
               int *status ) {

DECLARE_CHARACTER_DYN(PARAM);
DECLARE_CHARACTER_DYN(TYPE);
DECLARE_INTEGER(NDIMS);
DECLARE_INTEGER_ARRAY_DYN(DIMS);
DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_CREATE_EXPORT_CHARACTER( type, TYPE );
   F77_EXPORT_INTEGER( ndims, NDIMS );
   F77_CREATE_INTEGER_ARRAY( DIMS, ndims );
   F77_EXPORT_INTEGER_ARRAY( dims, DIMS, ndims );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(dat_creat)( CHARACTER_ARG(PARAM),
                        CHARACTER_ARG(TYPE),
                        INTEGER_ARG(&NDIMS),
                        INTEGER_ARRAY_ARG(DIMS),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(PARAM)
                        TRAIL_ARG(TYPE) ); )

   F77_FREE_CHARACTER( PARAM );
   F77_FREE_CHARACTER( TYPE );
   F77_FREE_INTEGER( DIMS );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}






