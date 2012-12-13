#define _POSIX_SOURCE 1		 /* Declare POSIX source */
/*
*+
*  Name:
*     ary.c

*  Purpose:
*     Implement the public C interface to the standalone ARY_ library.

*  Language:
*     ANSI C

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the standalone ARY_ library.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     <{enter_new_authors_here}>

*  History:
*    13-DEC-2012 (DSB):
*       Original version, based on ndf.c. Needs to be extended as
*       further ARY routines are needed.
*     <{enter_further_changes_here}>
*-
*/

/* Header files. */
/* ============= */
/* C run-time library header files. */
#include <ctype.h>               /* Character class tests */
#include <stdlib.h>              /* Utility functions */
#include <string.h>              /* String handling */

/* External interface header files. */
#include "ast.h"                 /* AST world coordinate system handling */
#include "dat_par.h"             /* Hierarchical Data System (HDS) */
#include "f77.h"                 /* C<-->Fortran interface macros */
#include "sae_par.h"             /* SAI__OK */
#include "ems.h"                 /* ems prototypes */

/* HDS Fortran Locator export/import routines */
#include "star/hds_fortran.h"

/* Internal header files. */
#include "ary.h"                 /* ARY_ library public interface */

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(ary_annul)( INTEGER(iary),
                           INTEGER(status) );

void aryAnnul( int *iary,
               int *status ) {

DECLARE_INTEGER(fiary);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *iary, fiary );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ary_annul)( INTEGER_ARG(&fiary),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fiary, *iary );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ary_find)( CHARACTER(loc),
                          CHARACTER(name),
                          INTEGER(iary),
                          INTEGER(status)
                          TRAIL(loc)
                          TRAIL(name) );

void aryFind( const HDSLoc *loc,
              const char *name,
              int *iary,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER_DYN(fname);
DECLARE_INTEGER(fiary);
DECLARE_INTEGER(fstatus);

   if ( loc == NULL ) {
      F77_EXPORT_LOCATOR( DAT__ROOT, floc );
   } else {
      if (*status == SAI__OK) {
	HDS_EXPORT_CLOCATOR( loc, floc, status );
	if (*status != SAI__OK) {
	  emsSetc("F",name);
	  emsRep("aryFind_err",
		 "aryFind: Error opening file ^F", status);
	}
      }
   }
   F77_CREATE_EXPORT_CHARACTER( name, fname );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ary_find)( CHARACTER_ARG(floc),
                       CHARACTER_ARG(fname),
                       INTEGER_ARG(&fiary),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc)
                       TRAIL_ARG(fname) ); )

   F77_FREE_CHARACTER( fname );
   F77_IMPORT_INTEGER( fiary, *iary );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ary_map)( INTEGER(iary),
                         CHARACTER(type),
                         CHARACTER(mmod),
                         POINTER_ARRAY(pntr),
                         INTEGER(el),
                         INTEGER(status)
                         TRAIL(type)
                         TRAIL(mmod) );

void aryMap( int iary,
             const char *type,
             const char *mmod,
             void *pntr[],
             int *el,
             int *status ) {

DECLARE_INTEGER(fiary);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_CHARACTER_DYN(fmmod);
DECLARE_POINTER_ARRAY_DYN(fpntr);
DECLARE_INTEGER(fel);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( iary, fiary );
   F77_CREATE_EXPORT_CHARACTER( type, ftype );
   F77_CREATE_EXPORT_CHARACTER( mmod, fmmod );
   F77_ASSOC_POINTER_ARRAY( fpntr, pntr );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ary_map)( INTEGER_ARG(&fiary),
                      CHARACTER_ARG(ftype),
                      CHARACTER_ARG(fmmod),
                      POINTER_ARRAY_ARG(fpntr),
                      INTEGER_ARG(&fel),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(ftype)
                      TRAIL_ARG(fmmod) ); )

   F77_FREE_CHARACTER( ftype );
   F77_FREE_CHARACTER( fmmod );
   F77_FREE_POINTER( fpntr );
   F77_IMPORT_INTEGER( fel, *el );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ary_sect)( INTEGER(iary1),
                          INTEGER(ndim),
                          INTEGER_ARRAY(lbnd),
                          INTEGER_ARRAY(ubnd),
                          INTEGER(iary2),
                          INTEGER(status) );

void arySect( int iary1,
              int ndim,
              const int lbnd[],
              const int ubnd[],
              int *iary2,
              int *status ) {

DECLARE_INTEGER(fiary1);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(flbnd);
DECLARE_INTEGER_ARRAY_DYN(fubnd);
DECLARE_INTEGER(fiary2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( iary1, fiary1 );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( flbnd, ndim );
   F77_EXPORT_INTEGER_ARRAY( lbnd, flbnd, ndim );
   F77_CREATE_INTEGER_ARRAY( fubnd, ndim );
   F77_EXPORT_INTEGER_ARRAY( ubnd, fubnd, ndim );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ary_sect)( INTEGER_ARG(&fiary1),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(flbnd),
                       INTEGER_ARRAY_ARG(fubnd),
                       INTEGER_ARG(&fiary2),
                       INTEGER_ARG(&fstatus) ); )

   F77_FREE_INTEGER( flbnd );
   F77_FREE_INTEGER( fubnd );
   F77_IMPORT_INTEGER( fiary2, *iary2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ary_size)( INTEGER(iary),
                          INTEGER(npix),
                          INTEGER(status) );

void arySize( int iary,
              int *npix,
              int *status ) {

DECLARE_INTEGER(fiary);
DECLARE_INTEGER(fnpix);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( iary, fiary );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ary_size)( INTEGER_ARG(&fiary),
                       INTEGER_ARG(&fnpix),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fnpix, *npix );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

