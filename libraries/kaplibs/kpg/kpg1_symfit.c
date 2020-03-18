#include <string.h>
#include "f77.h"
#include "kaplibs.h"
#include "mers.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg1_symfit)( CHARACTER(TYPE), INTEGER8(N), const void *x,
                             const void *y, LOGICAL(CLIP), DOUBLE(M), DOUBLE(B),
                             DOUBLE(RMS), INTEGER(STATUS) TRAIL(TYPE) ) {
/*
*+
*  Name:
*     KPG1_SYMFIT

*  Purpose:
*     Do a symetric least squares linear fit between two arrays

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG_SYMFIT( TYPE, N, X, Y, CLIP, M, B, RMS, STATUS )

*  Description:
*     This routine does a least squares linear fit on the X and Y arrays
*     to return "m" and "b" where the best fitting line is:
*
*        Y = m*X + b
*
*     The fit done by this function minimises the sum of the squared
*     residuals between the points and the fitted line. The residual used
*     is the distance from a point to the nearest point on the fitted
*     line. This is different to other similar function that use the vertical
*     (Y) displacement between the point and the line as the residual.
*     This means that the fit produced by this function is symetric -
*     fitting the Y values to the X values should give the algebraic inversion
*     of the line produced by fitting X values to Y values. This is not
*     generally true for fitting functions that use the Y displacement as
*     the residual.

*  Arguments:
*     TYPE = CHARACTER*(*) (Given)
*        The HDS type of the DATA array. Must be one of "_BYTE",
*        "_DOUBLE", "_INTEGER", "_INT64", "_REAL", "_WORD".
*     N = INTEGER*8 (Given)
*        The length of the X and Y arrays.
*     X( N ) = ? (Given)
*        Array of X values.
*     Y( N ) = ? (Given)
*        Array of Y values.
*     CLIP = LOGICAL (Given)
*        If .TRUE., outliers are identified and excluded from the fit.
*        Otherwise the fit includes all points. The initial identification
*        of outliers is performed by 3 iterations of sigma-clipping. The
*        first iteration clips the data at 1 sigma - subsequent iterations
*        clip at 3 sigma. Note, points rejected on an earlier iteration
*        may be re-instated on a lter iteration if they are found to lie
*        sufficiently close to the current best fit line.
*     M = DOUBLE PRECISION (Returned)
*        Returned holding the value of constant "m". VAL__BADD is returned
*        if it cannot be determined.
*     B = DOUBLE PRECISION (Returned)
*        Returned holding the value of constant "b". VAL__BADD is returned
*        if it cannot be determined.
*     RMS = DOUBLE PRECISION (Returned)
*        Returned holding the RMS residual. VAL__BADD is returned if it cannot
*        be determined. Note, any clipped points are excluded from this
*        estimate of the RMS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
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
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     18-MAR-2020 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_CHARACTER(TYPE)
   GENPTR_INTEGER8(N)
   GENPTR_LOGICAL(CLIP)
   GENPTR_DOUBLE(M)
   GENPTR_DOUBLE(B)
   GENPTR_DOUBLE(RMS)
   GENPTR_INTEGER(STATUS)

/* Call the appropriate routine to compute the statistics. */
   if( !strncmp( TYPE, "_BYTE", 5 ) ) {
      kpg1SymFitB( *N, x, y, F77_ISTRUE( *CLIP ), M, B, RMS, STATUS );

   } else if( !strncmp( TYPE, "_DOUBLE", 7 ) ) {
      kpg1SymFitD( *N, x, y, F77_ISTRUE( *CLIP ), M, B, RMS, STATUS );

   } else if( !strncmp( TYPE, "_INTEGER", 8 ) ) {
      kpg1SymFitI( *N, x, y, F77_ISTRUE( *CLIP ), M, B, RMS, STATUS );

   } else if( !strncmp( TYPE, "_INT64", 6 ) ) {
      kpg1SymFitK( *N, x, y, F77_ISTRUE( *CLIP ), M, B, RMS, STATUS );

   } else if( !strncmp( TYPE, "_REAL", 5 ) ) {
      kpg1SymFitF( *N, x, y, F77_ISTRUE( *CLIP ), M, B, RMS, STATUS );

   } else if( !strncmp( TYPE, "_WORD", 5 ) ) {
      kpg1SymFitW( *N, x, y, F77_ISTRUE( *CLIP ), M, B, RMS, STATUS );

   } else if( *STATUS == SAI__OK ){
      *STATUS = SAI__ERROR;
      errRepf( " ", "KPG1_SYMFIT: Unknown HDS data type '%.*s'.", STATUS,
               TYPE_length, TYPE );
   }

}
