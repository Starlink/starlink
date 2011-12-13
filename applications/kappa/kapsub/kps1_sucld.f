      SUBROUTINE KPS1_SUCLD( EL, FIT, RMS, CLIP, X, Y, Z, W, NBIN,
     :                         STATUS )
*+
*  Name:
*     KPS1_SUCLx

*  Purpose:
*     Rejects deviant surface-fit residuals truncating the given
*     list of unfitted values, weights and positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SUCLx( EL, FIT, RMS, CLIP, X, Y, Z, W, NBIN, STATUS )

*  Description:
*     Theis routine compares an array of fitted to corresponding raw
*     values.  Elements that are more than a defined number of standard
*     deviations away from the fit are removed from the raw data array.
*     It also truncates the corresponding arrays of weights and
*     positions.

*  Arguments:
*     EL = INTEGER (Returned)
*        The number of values in the list before the clipping operation.
*     FIT( EL ) = ? (Given and Returned)
*        The fitted values.
*     RMS = REAL (Returned)
*        The rms difference between the raw array and the fitted array.
*     CLIP = REAL (Given)
*        The standard deviation/rms threshold to define the clipping of
*        the raw data.  Thus a value of 3.0 would eliminate points
*        whose residuals from the fit lie outside the range -3.*rms
*        to +3.*sigma.
*     X( EL ) = ? (Given and Returned)
*        The x co-ordinates of the data.
*     Y( EL ) = ? (Given and Returned)
*        The y co-ordinates of the data.
*     Z( EL ) = ? (Given and Returned)
*        The values at the given x-y positions before fitting.
*     W( EL ) = ? (Given and Returned)
*        The weights at the given x-y positions.
*     NBIN = INTEGER (Returned)
*        The number of values left in the list after the clipping
*        operation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for double precision or real data types:
*     replace "x" in the routine name by D or R as appropriate.  The
*     FIT, X, Y, Z, and W arguments supplied to the routine must have
*     the data type specified.

*  Algorithm:
*     -  Loop for all points in the list
*     -  If the residual lies within the clipping range, increment the
*        count of bins copied and copy the value, weight and position.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 February 1 (MJC):
*        Original version.
*     1996 October 24 (MJC):
*        Made generic and renamed from CLPXYL.  Used modern style.
*        Added Description.  Swapped FIT and EL arguments.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL                 ! Number of data points before clipping
      DOUBLE PRECISION FIT( EL )           ! Fitted data
      REAL RMS                   ! RMS difference between the fitted and
                                 ! unfitted arrays
      REAL CLIP                  ! Clipping factor

*  Arguments Given and Returned:
      DOUBLE PRECISION X( EL )             ! X co-ordinates of the data
      DOUBLE PRECISION Y( EL )             ! Y co-ordinates of the data
      DOUBLE PRECISION Z( EL )             ! Data values
      DOUBLE PRECISION W( EL )             ! Weights of the data

*  Arguments Returned:
      INTEGER NBIN               ! Number of bins remaining after
                                 ! clipping

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL CLIPA                 ! Absolute clipping factor
      INTEGER I                  ! Loop counter
      REAL RESID                 ! Residual of the fit at a point
      REAL RMSA                  ! Absolute rms

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise absolute values and counter.
      CLIPA = ABS( CLIP )
      RMSA = ABS( RMS )
      NBIN = 0

*  Check each value in the list.
      DO I = 1, EL

*  Form the residual from the fit.
         RESID = REAL( Z( I ) - FIT( I ) )

*  Test to see the value lies within the allowed bounds.
         IF ( RESID .GT. -CLIPA * RMSA .AND.
     :        RESID .LT. CLIPA * RMSA ) THEN

*  Value lies between the clipping bounds, and so is retained.  Copy
*  the data from in former location in the list, to its new position in
*  the truncated list.
            NBIN = NBIN + 1
            Z( NBIN ) = Z( I )
            W( NBIN ) = W( I )
            X( NBIN ) = X( I )
            Y( NBIN ) = Y( I )
         END IF
      END DO

      END
