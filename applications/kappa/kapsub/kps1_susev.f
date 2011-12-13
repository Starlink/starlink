      SUBROUTINE KPS1_SUSEV( DIM1, DIM2, INARR, NXKNOT, NYKNOT,
     :                       XKNOT, YKNOT, NCOEF, COEFF, SCALE, NPOINT,
     :                       POINT, WORK, IWORK, OUTARR, RMS, STATUS )

*+
*  Name:
*     KPS1_SUSEV

*  Purpose:
*     Evaluates a bi-cubic spline for all elements of a two-dimensional
*     data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SUSEV( DIM1, DIM2, INARR, NXKNOT, NYKNOT, XKNOT, YKNOT,
*                      NCOEF, COEFF, SCALE, NPOINT, POINT, WORK, IWORK,
*                      OUTARR, RMS, STATUS )

*  Description:
*     This routine fills a two-dimensional array by evaluating a
*     supplied bi-cubic spline (in its a B-spline representation) at
*     each pixel's position.  The rms difference between the fitted,
*     interpolated pixels and the original pixels is also calculated.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the data arrays.
*     DIM2 = INTEGER (Given)
*        The second dimension of the data arrays.
*     INARR( DIM1, DIM2 ) = REAL (Given)
*        The original array without fitting.
*     NXKNOT = INTEGER (Given)
*        The number of interior knots in the x direction.
*     NYKNOT = INTEGER (Given)
*        The number of interior knots in the y direction.
*     XKNOT( NXKNOT+8 ) = REAL (Given)
*        The x positions of complete set of knots associated with x.
*     YKNOT( NYKNOT+8 ) = REAL (Given)
*        The y positions of complete set of knots associated with y.
*     NCOEF = INTEGER (Given)
*        The number of bi-cubic coefficients.  Must equal (%NXKNOT+4) *
*        (%NYKNOT+4).
*     COEFF( NCOEF ) = REAL (Given)
*        The bi-cubic B-spline coefficients, defined at the knots in the
*        order increasing x knot, then increasing y.  Thus coefficient
*        Cij in the standard convention is %COEFF((i-1)*(%NYKNOT+8)+j).
*     SCALE = REAL (Given)
*        The scale factor applied to the data values before calculating
*        the spline. The inverse of this value is used to correctly
*        scale the fitted values. If it is negative there will be no
*        rescaling.
*     NPOINT = INTEGER (Given)
*        The actual dimension of %POINT as declared.  Must be at least
*        4 * ( DIM1 + 1 ).
*     POINT( NPOINT ) = REAL (Returned)
*        Workspace for intermediate results.
*     WORK( DIM1, 2 ) = REAL (Returned)
*        Workspace for intermediate results.
*     IWORK( DIM1 + 1 ) = INTEGER (Returned)
*        Workspace.
*     OUTARR( DIM1, DIM2 ) = REAL (Returned)
*        The fitted array.
*     RMS = REAL (Returned)
*        The rms difference between the raw array and the fitted array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Algorithm:
*     -  Calculate the number of panels produced by the knots.
*     -  Initialise the rms sums.
*     -  Scan through the bins, calculating the co-ordinates of each
*        pixel, done a line at a time.
*     -  Evaluate the spline surface at the points of the line, scaling
*        the result if requested.
*     -  Form the sums for the rms error of the fit.
*     -  At the end of the loop calculate the rms error.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     1990 January 30 (MJC):
*        Original version.
*     1996 October 14 (MJC):
*        Replaced remaining NAG so changed the d.p. types to real, and
*        added IWORK argument.  Renamed from SPL2EV.  Converted to
*        modern style.
*     2006 April 12 (MJC):
*        Remove unused variables.
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
      INTEGER NXKNOT             ! Total number of interior knots in x
      INTEGER NYKNOT             ! Total number of interior knots in y
      REAL XKNOT( NXKNOT + 8 )   ! Positions of the knots in x
      REAL YKNOT( NYKNOT + 8 )   ! Positions of the knots in y
      INTEGER NCOEF              ! Number of spline coefficients
      REAL COEFF( NCOEF )        ! B-spline coefficients
      REAL SCALE                 ! Data scale factor
      INTEGER NPOINT             ! Dimension of POINT

*  Arguments Returned:
      REAL POINT( NPOINT )    ! Workspace
      REAL WORK( DIM1, 2 )       ! Workspace to store fitted values and
                                 ! positions
      INTEGER IWORK( DIM1 + 1 )  ! Workspace
      REAL OUTARR( DIM1, DIM2 )  ! Fitted data
      REAL RMS                   ! RMS difference between the fitted and
                                 ! unfitted arrays

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER IFAIL              ! PDA error status
      INTEGER J                  ! Loop counter
      INTEGER NPT                ! Number of points used to calculate
                                 ! the rms
      REAL SUMSQ                 ! Sum of the square of differences
      LOGICAL SCALED             ! Data are scaled
      REAL Y                     ! Y co-ordinate of an array line

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      SCALED = SCALE .GT. 0.0

*  Initialise sums to form the rms error of the fit.
      SUMSQ = 0.0
      NPT = 0

*  Scan through the bins, calculating the x and y co-ordinates for each
*  pixel.  Note for efficiency reasons the fitting for a whole line is
*  made in a single call.
      DO J = 1, DIM2
         Y = REAL( J ) - 0.5
         DO I = 1, DIM1
            WORK( I, 2 ) = REAL( I ) - 0.5
         END DO

*  Evaluate the fitted surface for pixels in the line.  By definition
*  we are using a bi-cubic.
         CALL PDA_BISPEV( XKNOT, NXKNOT + 8, YKNOT, NYKNOT + 8, COEFF,
     :                    3, 3, WORK( 1, 2 ), DIM1, Y, 1, WORK( 1, 1 ),
     :                    POINT, NPOINT, IWORK, DIM1 + 1, IFAIL,
     :                    STATUS )

*  Test for an error.
         IF ( IFAIL .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'IFAIL', IFAIL )
            CALL ERR_REP( 'KPS1_SUSEV_PDA',
     :        'KPS1_SUSEV: Error ^IFAIL returned by PDA_BISPEV '/
     :        /'evaluation.', STATUS )
            GO TO 999
         END IF

*  Store the results in the output array, re-scaling if necessary.
         IF ( SCALED ) THEN
            DO I = 1, DIM1
               OUTARR( I, J ) = WORK( I, 1 ) / SCALE
            END DO
         ELSE
            DO I = 1, DIM1
               OUTARR( I, J ) = WORK( I, 1 )
            END DO
         END IF

*  Form sums for the rms error of the fit.  Exclude bad pixels.
         DO I = 1, DIM1
            IF ( INARR( I, J ) .NE. VAL__BADR ) THEN
               SUMSQ = SUMSQ + ( OUTARR( I, J ) - INARR( I, J ) ) ** 2
               NPT = NPT + 1
            END IF
         END DO

*  End of the loop through the line of pixels.
      END DO

*  Calculate the rms error of the fit.
      IF ( NPT .GE. 1 ) THEN
         RMS = SQRT( SUMSQ / REAL( NPT ) )
      ELSE
         RMS = VAL__BADR
      END IF

  999 CONTINUE

      END
