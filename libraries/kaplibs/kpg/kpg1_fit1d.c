#include "f77.h"
#include "kaplibs.h"

F77_SUBROUTINE(kpg1_fit1d)( INTEGER(LBND), INTEGER(UBND), DOUBLE_ARRAY(Y),
                            DOUBLE_ARRAY(X), DOUBLE(M), DOUBLE(C), DOUBLE(RMS),
                            INTEGER(STATUS) ){
/*
*  Name:
*     KPG1_FIT1D

*  Purpose:
*     Fits a least-squares straight line to supplied data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_FIT1D( LBND, UBND, Y, X, M, C, RMS, STATUS )

*  Description:
*     A straight line is fitted to the data supplied in X and Y, using
*     the least-squares criterion. The returned values of M and C are
*     the gradient and intercept of the fit, so that y = M.x + C. The
*     RMS residual of the Y data from the fit is returned in RMS.
*
*     An error is reported if there are less than two good data values
*     in Y, or if the X values cover a range of zero.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the X and Y arrays.
*     UBND = INTEGER (Given)
*        The upper bound of the X and Y arrays.
*     Y( LBND : UBND ) = DOUBLE PRECISION (Given)
*        The Y data values. Any bad values are ignored.
*     X( LBND : UBND ) = DOUBLE PRECISION (Given)
*        The X positions corresponding to each Y data value.
*     M = DOUBLE PRECISION (Returned)
*        The gradient.
*     C = DOUBLE PRECISION (Returned)
*        The intercept.
*     RMS = DOUBLE PRECISION (Returned)
*        The RMS residual between the Y values and the fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1992 (DSB):
*        Original version (IRM_FIT1D).
*     7-DEC-1998 (DSB):
*        Brought into KAPPA from IRAS90.
*     8-JAN-2010 (DSB):
*        Re-written in C.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(LBND)
   GENPTR_INTEGER(UBND)
   GENPTR_DOUBLE_ARRAY(Y)
   GENPTR_DOUBLE_ARRAY(X)
   GENPTR_DOUBLE(M)
   GENPTR_DOUBLE(C)
   GENPTR_DOUBLE(RMS)
   GENPTR_INTEGER(STATUS)
   double m, c, rms;

   kpg1Fit1d( *LBND, *UBND, Y, X, &m, &c, &rms, STATUS );
   *M = m;
   *C = c;
   *RMS = rms;
}
