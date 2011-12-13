      SUBROUTINE KPS1_PSDIM( AXISR, THETA, FWHM, GAMMA, CUT, PSFDIM,
     :                       STATUS )
*+
*  Name:
*     KPS1_PSDIM

*  Purpose:
*     Finds the dimensions of a 2-dimensional point-spread function
*     array to a given threshold.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PSDIM( AXISR, THETA, FWHM, GAMMA, CUT, PSFDIM, STATUS )

*  Description:
*     This routine calculates for the PSF application the dimensions of
*     the array needed to encompass a point-spread function of the
*     following radial form:
*        D =  A exp(-0.5 * (r/sigma) ** gamma )
*     where r is calculated from the true radial distance from the star
*     centre allowing for image ellipticity, sigma is the Gaussian
*     precision constant or profile width.  The point-spread function
*     may be oriented at an arbitrary angle.

*  Arguments:
*     AXISR = REAL (Given)
*        The axis ratio of the point-spread function.
*     THETA = REAL (Given)
*        The orientation of the major axis of the point-spread function
*        to the x axis in radians (x through y positive).
*     FWHM = REAL (Given)
*        The full width at half maximum of the point-spread function in
*        the minor-axis direction.
*     GAMMA = REAL (Given)
*        The exponent in the radial point-spread function.
*     CUT = REAL (Given)
*        The threshold which must just be included in the array,
*        normalised by the central point-spread function amplitude.
*        Thus 0.5 is equivalent to the full-width half maximum.
*        Acceptable values lie in the range must be greater than 0 and
*        less than 1.
*     PSFDIM( 2 ) = INTEGER (Returned)
*        The dimensions of the array that will just (to the next
*        exterior array element) encompass the threshold value.  The
*        dimensions will always be odd, so the centre of the PSF is
*        within the central element.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     1991 July 9 (MJC):
*        Original version.
*     1995 April 5 (MJC):
*        Fixed bug in evaluation of the dimension sizes.  Used the
*        modern style variable declaration and comment indentation.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL AXISR
      REAL CUT
      REAL FWHM
      REAL GAMMA
      REAL THETA

*  Arguments Returned:
      INTEGER PSFDIM( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL K( 4 )                ! Constants in the polar co-ordinate
                                 ! description of the ellipse
      REAL MAJOR                 ! Displacement of the threshold from
                                 ! the centre of the PSF along the
                                 ! major axis
      REAL MINOR                 ! Displacement of the threshold from
                                 ! the centre of the PSF along the
                                 ! minor axis
      REAL RXMAX                 ! Polar angle of the ellipse for which
                                 ! the x co-ordinate is largest
      REAL RYMAX                 ! Polar angle of the ellipse for which
                                 ! the y co-ordinate is largest
      REAL SIGMA                 ! The Gaussian width along the minor
                                 ! axis
      REAL XL                    ! Larger x displacement from the
                                 ! centre of the PSF
      REAL YL                    ! Larger y displacement from the
                                 ! centre of the PSF
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert from FWHM to standard deviation.
      SIGMA = ( 0.5 * FWHM ) / 1.38629 ** ( 1.0 / GAMMA )

*  Find the position along the major axis that corresponds to the
*  threshold by solving for the deviate given a cut.  FWHM is along the
*  minor axis so multiply by the axis ratio.
      MAJOR = SIGMA * AXISR * SQRT( 2.0 * LOG( 1.0 / CUT ) )

*  Find the position along the minor axis that corresponds to the
*  threshold.
      MINOR = SIGMA * SQRT( 2.0 * LOG( 1.0 / CUT ) )

*  Find the maximum x and y co-ordinates of the ellipse oriented at this
*  angle.  First define some constants to parameterise the ellipse.
      K( 1 ) = MAJOR * COS( THETA )
      K( 3 ) = MAJOR * SIN( THETA )
      K( 2 ) = MINOR * SIN( THETA )
      K( 4 ) = MINOR * COS( THETA )

*  Find the orientation in polar co-ordinates of the points in the
*  locus that correspond to the maximum x and y co-ordinates.  This is
*  done by differentiating the x-y co-ordinates with respect to the
*  polar orientation and setting these to zero.
      RXMAX = ATAN2( K( 2 ), -K( 1 ) )
      RYMAX = ATAN2( K( 4 ), K( 3 ) )

*  Hence evaluate the maximum Cartesian co-ordinates.  These
*  expressions assume the centre of the ellipse is at (0,0).  The ABS
*  is probably not needed, but it's safer to have it.
      XL = ABS( K( 1 ) * COS( RXMAX ) - K( 2 ) * SIN( RXMAX ) )
      YL = ABS( K( 3 ) * COS( RYMAX ) + K( 4 ) * SIN( RYMAX ) )

*  Evaluate the dimensions.  Ensure an odd number of elements and that
*  the threshold value lies interior the the elements' centres.
      PSFDIM( 1 ) = ( INT( XL ) + 1 ) * 2 + 1
      PSFDIM( 2 ) = ( INT( YL ) + 1 ) * 2 + 1

      END
