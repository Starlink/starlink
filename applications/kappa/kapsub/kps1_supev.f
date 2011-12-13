      SUBROUTINE KPS1_SUPEV( DIM1, DIM2, INARR, XMIN, XMAX, YMIN, YMAX,
     :                       NXPAR, NYPAR, MCHOEF, CHCOEF, WORK,
     :                       OUTARR, RMS, STATUS )
*+
*  Name:
*     KPS1_SUPEV

*  Purpose:
*     Evaluates a bivariate Chebyshev polynomial series for all elements
*     of a 2-d data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SUPEV( DIM1, DIM2, INARR, XMIN, XMAX, YMIN, YMAX,
*                      NXPAR, NYPAR, MXCOEF, CHCOEF, WORK,
*                      OUTARR, RMS, STATUS )

*  Description:
*     This routine fills a 2-dimensional array by evaluating a supplied
*     bivariate Chebyshev polynomial at each pixel's position.  The rms
*     difference between the fitted pixels and the original pixels is
*     also calculated.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the data arrays.
*     DIM2 = INTEGER (Given)
*        The second dimension of the data arrays.
*     INARR( DIM1, DIM2 ) = REAL (Given)
*        The original array without fitting.
*     XMIN = DOUBLE PRECISION (Given)
*        Lower end of the x range of the fit. It must not be greater
*        than the x position of the first pixel in the data array.
*     YMIN = DOUBLE PRECISION (Given)
*        Lower end of the y range of the fit. It must not be greater
*        than the y position of the first pixel in the data array.
*     XMAX = DOUBLE PRECISION (Given)
*        Upper end of the x range of the fit. It must not be less
*        than the x position of the last pixel in the data array.
*     YMAX = DOUBLE PRECISION (Given)
*        Lower end of the y range of the fit. It must not be less
*        than the y position of the last pixel in the data array.
*     NXPAR = INTEGER (Given)
*        The number of parameters of the FIT in the x direction, i.e
*        the degree of the polynomial plus one.
*     NYPAR = INTEGER (Given)
*        The number of parameters of the FIT in the y direction, i.e
*        the degree of the polynomial plus one.
*     MCHOEF = INTEGER (Given)
*        The dimension of the array of Chebyshev coefficients.
*     CHCOEF( MCHOEF ) = DOUBLE PRECISION (Given)
*        The Chebyshev polynomial coefficients, in the order increasing
*        x power for each increasing y power.  Thus coefficient Aij in
*        the standard convention is %CHCOEF(i*(%NYPAR)+j+1). The array
*        may be rectangular, i.e. the highest x and y orders do not
*        have to be the same.
*     WORK( DIM1, 2 ) = DOUBLE PRECISION (Returned)
*        Workspace for intermediate results.
*     OUTARR( DIM1, DIM2 ) = REAL (Returned)
*        The fitted array.
*     RMS = REAL (Returned)
*        The rms difference between the raw array and the fitted array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Algorithm:
*     -  Initialise the rms sums.
*     -  Scan through the bins, calculating the co-ordinates of each
*        pixel, done a line at a time.
*     -  Evaluate the Chebyshev surface for the line of pixels.
*     -  Form the sums for the rms error of the fit.
*     -  At the end of the loop calculate the rms error.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 January 30 (MJC):
*        Original version.
*     1996 October 8 (MJC):
*        Removed NAG.  Modern style.  Renamed from PLY2EV.
*     2011 May 11 (MJC):
*        Removed no-longer-used argument NCOEF.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad-pixel definitions

*  Arguments Given:
      INTEGER DIM1, DIM2         ! Dimensions of the data arrays
      REAL INARR( DIM1, DIM2 )   ! Raw, unfitted data
      DOUBLE PRECISION XMIN, XMAX ! X bounds of the fit
      DOUBLE PRECISION YMIN, YMAX ! Y bounds of the fit
      INTEGER NXPAR              ! X degree of the polynomial plus 1
      INTEGER NYPAR              ! Y degree of the polynomial plus 1
      INTEGER MCHOEF             ! Dimension of Chebyshev coeff. array
      DOUBLE PRECISION CHCOEF( MCHOEF ) ! Chebyshev coefficients

*  Arguments Returned:
      DOUBLE PRECISION WORK( DIM1, 2 ) ! Workspace to store fitted
                                 ! values and positions
      REAL OUTARR( DIM1, DIM2 )  ! Fitted data
      REAL RMS                   ! RMS difference between the fitted and
                                 ! unfitted arrays

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXPAR              ! Maximum number of parameters which
                                 ! can be handled in each direction
      PARAMETER ( MXPAR = 15 )

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER NPT                ! Number of points used to calculate
                                 ! the rms
      DOUBLE PRECISION PX( MXPAR ) ! Work array
      REAL SUMSQ                 ! Sum of the square of differences
      DOUBLE PRECISION Y         ! Y co-ordinate

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise sums to form the rms error of the fit.
      SUMSQ = 0.0
      NPT = 0

*  Scan through the bins, calculating the x and y co-ordinates for each
*  pixel.  Note for efficiency reasons the fitting for a whole line is
*  made in a single call.
      DO J = 1, DIM2
         Y = DBLE( J ) - 0.5D0
         DO I = 1, DIM1
            WORK( I, 2 ) = DBLE( I ) - 0.5D0
         END DO

*  Evaluate the fitted surface for all pixels in the line.
         CALL KPG1_CHE2D( DIM1, XMIN, XMAX, WORK( 1, 2 ), YMIN, YMAX, Y,
     :                    NXPAR - 1, NYPAR - 1, MCHOEF, CHCOEF, MXPAR,
     :                    PX, WORK( 1, 1 ), STATUS )

*  Copy the values to the real output array from the double-precision
*  work array.
         DO I = 1, DIM1
            OUTARR( I, J ) = REAL( WORK( I, 1 ) )
         END DO

*  Form sums for the rms error of the fit.
         DO  I = 1, DIM1
            IF ( INARR( I, J ) .NE. VAL__BADR ) THEN
               SUMSQ = SUMSQ + ( OUTARR( I, J ) - INARR( I, J ) ) ** 2
               NPT = NPT + 1
            END IF
         END DO

*  End of the loops through the line of pixels.
      END DO

*  Calculate the rms error of the fit.
      IF ( NPT .GE. 1 ) THEN
         RMS = SQRT( SUMSQ / REAL( NPT ) )
      ELSE
         RMS = VAL__BADR
      END IF

      END
