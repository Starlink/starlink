      SUBROUTINE KPS1_SUSEI( DIM1, DIM2, INARR, IDX, IDY, NXKNOT,
     :                       NYKNOT, XKNOT, YKNOT, NCOEF, COEFF, SCALE,
     :                       OUTARR, RMS, STATUS )

*+
*  Name:
*     KPS1_SUSEI

*  Purpose:
*     Evaluates a bi-cubic spline for all elements of a 2-d data array
*     via interpolation of a coarse grid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SUSEI( DIM1, DIM2, INARR, IDX, IDY, NXKNOT, NYKNOT,
*                      XKNOT, YKNOT, NCOEF, COEFF, SCALE, OUTARR, RMS,
*                      STATUS )

*  Description:
*     This routine fills a 2-d array by evaluating a supplied bi-cubic
*     spline (in its a B-spline representation) in a coarse grid, and
*     then using bilinear interpolation to derive individual pixel
*     values. The rms difference between the fitted, interpolated pixels
*     and the original pixels is also calculated.

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the data arrays.
*     DIM2 = INTEGER (Given)
*        The second dimension of the data arrays.
*     INARR( DIM1, DIM2 ) = REAL (Given)
*        The original array without fitting.
*     IDX = INTEGER (Given)
*        The number of pixels in a coarse-grid bin in the x direction.
*        This is expected to be the same value as was used to bin the
*        array when deriving the polynomial fit.
*     IDY = INTEGER (Given)
*        The number of pixels in a coarse-grid bin in the y direction.
*        This is expected to be the same value as was used to bin the
*        array when deriving the polynomial fit.
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
*     OUTARR( DIM1, DIM2 ) = REAL (Returned)
*        The fitted array.
*     RMS = REAL (Returned)
*        The rms difference between the raw array and the
*        fitted array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses the magic-value method for bad or undefined pixels.

*  Algorithm:
*     -  Calculate the number of panels produced by the knots.
*     -  Initialise the rms sums.
*     -  Scan through the bins, calculating the co-ordinates of the
*        corners of each bin
*     -  Evaluate the spline surface at the corners of the bin.
*     -  Calculate the coefficients of a bi-linear surface which passes
*        through the four corners.
*     -  Scan through all pixels within the bin, evaluating their values
*        by bi-linear interpolation.
*     -  Scale the fit if requested.
*     -  Form the sums for the rms error of the fit.
*     -  At the end of the loop calculate the rms error.

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
*     1990 January 30 (MJC):
*        Original version based on EDRS code.
*     1996 October 14 (MJC):
*        Replaced remaining NAG so changed the d.p. types to real, and
*        no longer needed POINT and NPOINT arguments.  Renamed from
*        SPL2EI.  Converted to modern style.
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
      INTEGER IDX, IDY           ! Size of a coarse-grid bin in pixels
      INTEGER NXKNOT             ! Total number of interior knots in x
      INTEGER NYKNOT             ! Total number of interior knots in y
      REAL XKNOT( NXKNOT + 8 )   ! Positions of the knots in x
      REAL YKNOT( NYKNOT + 8 )   ! Positions of the knots in y
      INTEGER NCOEF              ! Number of spline coefficients
      REAL COEFF( NCOEF )        ! B-spline coefficients
      REAL SCALE                 ! Data scale factor

*  Arguments Returned:
      REAL OUTARR( DIM1, DIM2 )  ! Fitted data
      REAL RMS                   ! RMS difference between the fitted and
                                 ! unfitted arrays

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL BX, BY                ! Number of pixels in a bin in x and y
      REAL BXY                   ! Total number of pixels in a bin
      REAL DX, DY                ! X-y co-ordinates of a pixel within
                                 ! the current coarse bin
      REAL F( 4 )                ! Fitted values for the four corners of
                                 ! a coarse bin
      REAL FA, FB                ! Fractional positions for
                                 ! interpolation
      REAL FUN                   ! Interpolated pixel value
      INTEGER I                  ! Loop counter
      INTEGER IFAIL              ! PDA error status
      INTEGER IMIN, IMAX         ! X indices of the pixels defining the
                                 ! lower and upper bounds of the
                                 ! current coarse bin
      INTEGER IWORK( 4 )         ! Work array
      INTEGER J                  ! Loop counter
      INTEGER JMIN, JMAX         ! Y indices of the pixels defining the
                                 ! lower and upper bounds of the
                                 ! current coarse bin
      INTEGER NPT                ! Number of points used to calculate
                                 ! the rms
      LOGICAL SCALED             ! Data are to be scaled?
      REAL SUMSQ                 ! Sum of the square of differences
      REAL WORK( 16 )            ! Work array
      REAL X( 2 )                ! X co-ordinates of the coarse-bin's
                                 ! corners
      REAL Y( 2 )                ! Y co-ordinates of the coarse-bin's
                                 ! corners
      REAL ZA, ZB, ZC, ZD        ! Coefficients of a bilinear surface
                                 ! x-y offsets plus gradients
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Define some variables for efficiency.
      BX = REAL ( IDX )
      BY = REAL ( IDY )
      BXY = BX * BY

      SCALED = SCALE .GT. 0.0

*  Initialise sums to form the rms error of the fit.
      SUMSQ = 0.0
      NPT = 0

*  Scan through the bins, calculating the minimum and maximum x and y
*  co-ordinates for each bin.
      DO JMIN = 1, DIM2, IDY
         JMAX = MIN( JMIN + IDY - 1, DIM2 )

*  Find the y co-ordinates of the bin corners.
         Y( 1 ) = REAL( JMIN ) - 0.5
         Y( 2 ) = REAL( JMAX ) + 0.5

         DO IMIN = 1, DIM1, IDX
            IMAX = MIN( IMIN + IDX - 1, DIM1 )

*  Find the x co-ordinates of the bin corners.
            X( 1 ) = REAL( IMIN ) - 0.5
            X( 2 ) = REAL( IMAX ) + 0.5

*  Evaluate the fitted surface at the four bin corners (i.e. 2 by 2).
*  Since the number of points is four, the dimensions of the work arrays
*  are known and can be declared explicitly in this routine.  By
*  definition we are using a bi-cubic.
            CALL PDA_BISPEV( XKNOT, NXKNOT + 8, YKNOT, NYKNOT + 8,
     :                       COEFF, 3, 3, X, 2, Y, 2, F, WORK, 16,
     :                       IWORK, 4, IFAIL )

*  Test for an error.
            IF ( IFAIL .NE. 0 ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'IFAIL', IFAIL )
               CALL ERR_REP( 'KPS1_SUSEI_PDA',
     :           'KPS1_SUSEI: Error ^IFAIL returned by PDA_BISPEV '/
     :           /'evaluation.', STATUS )
               GO TO 999
            END IF

*  Calculate the coefficients of a bi-linear surface which passes
*  through the four corners.  Note that the values returned by
*  PDA_BISPEV are in the order: (X1,Y1), (X1,Y2), (X2,Y1), (X2,Y2).
            ZA = F( 1 )
            ZB = ( F( 3 ) - ZA ) / BX
            ZC = ( F( 2 ) - ZA ) / BY
            ZD = ( F( 4 ) - ZA - ZB * BX - ZC * BY ) / BXY

*  Now scan all pixels in the bin, evaluating the bi-linear surface.
            DY = -0.5

            DO J = JMIN, JMAX
               DY = DY + 1.0
               FA = ZA + ZC * DY
               FB = ZB + ZD * DY
               DX = -0.5

               DO I = IMIN, IMAX
                  DX = DX + 1.0
                  FUN = FA + FB * DX

*  Scale the data where needed.
                  IF ( SCALED ) THEN
                     OUTARR( I, J ) = FUN / SCALE
                  ELSE
                     OUTARR( I, J ) = FUN
                  END IF

*  Form sums for the rms error of the fit.
                  IF ( INARR( I, J ) .NE. VAL__BADR ) THEN
                     SUMSQ = SUMSQ +
     :                       ( OUTARR( I, J ) - INARR( I, J ) ) ** 2
                     NPT = NPT + 1
                  END IF
               END DO
            END DO

*  End of the loops through the bins.
         END DO
      END DO


*  Calculate the rms error of the fit.
      IF ( NPT .GE. 1 ) THEN
         RMS = SQRT( SUMSQ / REAL( NPT ) )
      ELSE
         RMS = VAL__BADR
      END IF

  999 CONTINUE

      END
