      SUBROUTINE KPS1_SUPEI( DIM1, DIM2, INARR, IDX, IDY, XMIN, XMAX,
     :                       YMIN, YMAX, NXPAR, NYPAR, MCHOEF, CHCOEF,
     :                       OUTARR, RMS, STATUS )

*+
*  Name:
*     KPS1_SUPEI

*  Purpose:
*     Evaluates a bivariate Chebyshev polynomial series for all elements
*     of a 2-d data array via interpolation of a coarse grid.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SUPEI( DIM1, DIM2, INARR, IDX, IDY, XMIN, XMAX, YMIN,
*                      YMAX, NXPAR, NYPAR, MCHOEF, CHCOEF,
*                      OUTARR, RMS, STATUS )

*  Description:
*     This routine fills a 2-dimensional array by evaluating a supplied
*     bivariate Chebyshev polynomial in a coarse grid, and then using
*     bilinear interpolation to derive individual pixel values.  The rms
*     difference between the fitted, interpolated pixels and the
*     original pixels is also calculated.

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
*     -  Scan through the pixels.
*     -  Calculate the x and y co-ordinates of the four corners of the
*        coarse-grid bin
*     -  Evaluate the Chebyshev surface at the four corners.
*     -  Calculate the coefficients of a bi-linear surface which passes
*        through all the corners.
*     -  Scan all the pixels in the bin, evaluating pixels by bilinear
*        interpolation.
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
*        Original version based on some EDRS code.
*     1996 October 8 (MJC):
*        Removed NAG.  Modern style.  Renamed from PLY2EI.
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
      INTEGER DIM1, DIM2        ! Dimensions of the data arrays
      REAL INARR( DIM1, DIM2 )   ! Raw, unfitted data
      INTEGER IDX, IDY           ! Size of a coarse-grid bin in pixels
      DOUBLE PRECISION XMIN, XMAX ! X bounds of the fit
      DOUBLE PRECISION YMIN, YMAX ! Y bounds of the fit
      INTEGER NXPAR              ! X degree of the polynomial plus 1
      INTEGER NYPAR              ! Y degree of the polynomial plus 1
      INTEGER MCHOEF             ! Dimension of Chebyshev coeff. array
      DOUBLE PRECISION CHCOEF( MCHOEF ) ! Chebyshev coefficients

*  Arguments Returned:
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
      REAL BX, BY                ! Number of pixels in a bin in x and y
      REAL BXY                   ! Total number of pixels in a bin
      REAL DX, DY                ! X-y co-ordinates of a pixel within
                                 ! the current coarse bin
      DOUBLE PRECISION F( 2 )    ! Fitted values for the four corners of
                                 ! a coarse bin (obtained in pairs)
      REAL FA, FB                ! Fractional positions for
                                 ! interpolation
      REAL FUN                   ! Interpolated pixel value
      INTEGER I, J               ! Loop counters
      INTEGER IMIN, IMAX         ! X indices of the pixels defining the
                                 ! lower and upper bounds of the
                                 ! current coarse bin
      INTEGER JMIN, JMAX         ! Y indices of the pixels defining the
                                 ! lower and upper bounds of the
                                 ! current coarse bin
      INTEGER NPT                ! Number of points used to calculate
                                 ! the rms
      DOUBLE PRECISION PX( MXPAR ) ! Work array
      REAL SUMSQ                 ! Sum of the square of differences
      DOUBLE PRECISION X( 2 )    ! X co-ordinates of the coarse-bin's
                                 ! vetical sides
      DOUBLE PRECISION Y( 2 )    ! Y co-ordinates of the coarse-bin's
                                 ! horizontal sides
      REAL ZA, ZB, ZC, ZD        ! Coefficients of a bilinear surface
                                 ! x,y offsets plus gradients

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Some variables defined for efficiency.
      BX = REAL ( IDX )
      BY = REAL ( IDY )
      BXY = BX * BY

*  Initialise sums to form the rms error of the fit.
      SUMSQ = 0.0
      NPT = 0

*  Scan through the bins, calculating the minimum and maximum x and y
*  co-ordinates for each bin.
      DO JMIN = 1, DIM2, IDY
         JMAX = MIN( JMIN + IDY - 1, DIM2 )

*  Find the y co-ordinates of the top and bottom sides of the bin.
         Y( 1 ) = DBLE( JMIN ) - 0.5D0
         Y( 2 ) = DBLE( JMAX ) + 0.5D0

         DO IMIN = 1, DIM1, IDX
            IMAX = MIN( IMIN + IDX - 1, DIM1 )

*  Find the x co-ordinates of the lower two corners.
            X( 1 ) = DBLE( IMIN ) - 0.5D0
            X( 2 ) = DBLE( IMAX ) + 0.5D0

*  Evaluate the fitted surface at the 2 bin corners.
            CALL KPG1_CHE2D( 2, XMIN, XMAX, X, YMIN, YMAX, Y( 1 ),
     :                       NXPAR - 1, NYPAR - 1, MCHOEF, CHCOEF,
     :                       MXPAR, PX, F, STATUS )

*  Calculate two of four coefficients of a bi-linear surface which
*  passes through the four corners.
            ZA = REAL( F( 1 ) )
            ZB = ( REAL( F( 2 ) ) - ZA ) / BX

*  Find the x co-ordinates of the upper two corners.
            X( 1 ) = DBLE( IMAX ) + 0.5D0
            X( 2 ) = DBLE( IMIN ) - 0.5D0

*  Evaluate the fitted surface at the other 2 bin corners.
            CALL KPG1_CHE2D( 2, XMIN, XMAX, X, YMIN, YMAX, Y( 2 ),
     :                       NXPAR - 1, NYPAR - 1, MCHOEF, CHCOEF,
     :                       MXPAR, PX, F, STATUS )

*  Calculate the second pair coefficients of a bi-linear surface which
*  passes through the four corners.
            ZC = ( REAL( F( 2 ) ) - ZA ) / BY
            ZD = ( REAL( F( 1 ) ) - ZA - ZB * BX - ZC * BY ) / BXY

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

                  OUTARR( I, J ) = FUN

*  Form sums for the rms error of the fit.
                  IF ( INARR( I, J ) .NE. VAL__BADR ) THEN
                     SUMSQ = SUMSQ + ( FUN - INARR( I, J ) ) ** 2
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

      END
