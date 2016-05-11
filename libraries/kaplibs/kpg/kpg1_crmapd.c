#include "f77.h"
#include "kaplibs.h"

F77_SUBROUTINE(kpg1_crmapd)( INTEGER(NX), INTEGER(NY), DOUBLE_ARRAY(X),
                            DOUBLE_ARRAY(Y), INTEGER(BOX), DOUBLE_ARRAY(R),
                            INTEGER(STATUS) ){
/*
*  Name:
*     KPG1_CRMAPD

*  Purpose:
*     Get an image of the local correlation coefficient between two images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CRMAPD( NX, NY, X, Y, BOX, R, STATUS )

*  Description:
*     This routine returns an array in which each pixel value is the
*     correlation coefficient between corresponding boxes of values in
*     two supplied arrays, centred on the output pixel.

*  Arguments:
*     NX = INTEGER (Given)
*        The number of pixels along each row of the suppied arrays.
*     NY = INTEGER (Given)
*        The number of pixels along each column of the suppied arrays.
*     X( NX, NY ) = DOUBLE PRECISION (Given)
*        The first image.
*     Y( NX, NY ) = DOUBLE PRECISION (Given)
*        The second image.
*     BOX = INTEGER (Given)
*        The linear size of the smoothing box, in pixels.
*     R( NX, NY ) = DOUBLE PRECISION (Returned)
*        Returned holding the map of correlation coefficients.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The three arays "X", "Y" and "R" must all be the same size.
*     - The current algorithm could be made much more efficient. It
*      currently re-evaluates the statistics from scratch for each
*      box of input pixels for each output pixel.

*  Copyright:
*     Copyright (C) 2016 East Asian Observatory.
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     11-MAY-2016 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(NX)
   GENPTR_INTEGER(NY)
   GENPTR_DOUBLE_ARRAY(Y)
   GENPTR_DOUBLE_ARRAY(X)
   GENPTR_INTEGER(BOX)
   GENPTR_DOUBLE_ARRAY(R)
   GENPTR_INTEGER(STATUS)

   kpg1CrMapD( *NX, *NY, X, Y, *BOX, R, STATUS );
}
