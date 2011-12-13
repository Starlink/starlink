      SUBROUTINE KPS1_LUCIM( N, NPIX, NLIN, WLIM, SNYDER, FILE_3,
     :                       FILE_4, FILE_8, FILE_1, FILE_7, FILE_2,
     :                       FILE_5, STATUS )
*+
*  Name:
*     KPS1_LUCIM

*  Purpose:
*     Creates the next estimate of the Richardson-Lucy reconstructed
*     image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LUCIM( N, NPIX, NLIN, WLIM, SNYDER, FILE_3, FILE_4,
*                      FILE_8, FILE_1, FILE_7, FILE_2, FILE_5, STATUS )

*  Description:
*     The current reconstruction image supplied in file <1> is updated
*     by a single iteration of the Richardson-Lucy algorithm, using the
*     simulated data supplied in file 7.  This involves forming the
*     correction factor for each data value (the ratio of observed to
*     simulated data value - but see argument SNYDER), then mapping
*     these correction factors into an image using the transposed PSF
*     to get the mean correction factor at each image pixel.  The
*     current reconstruction image is then multiplied by this image of
*     correction factors.

*  Arguments:
*     N = INTEGER (Given)
*        The number of elements in each internal file.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in each internal file.
*     NLIN = INTEGER (Given)
*        The number of lines in each internal file.
*     WLIM = REAL (Given)
*        The weight limit for good pixels.
*     SNYDER = LOGICAL (Given)
*        If .TRUE. of the Snyder modification to the basic R-L
*        algorithm is to be used.  This causes the correction factor
*        for each data value to be modified by adding the data value
*        variance to both the numerator and the denominator.  This is
*        used in the STSDAS LUCY program.
*     FILE_3( N ) = REAL (Given)
*        The Fourier transform of the PSF (NOT the transposed PSF).
*     FILE_4( N ) = REAL (Given)
*        The observed data.
*     FILE_8( N ) = REAL (Given)
*        The data variances.
*     FILE_1( N ) = REAL (Given and Returned)
*        The current reconstructed image.
*     FILE_7( N ) = REAL (Given and Returned)
*        Supplied holding simulated data created from the supplied
*        image in file 1.  Returned holding internal workings.
*     FILE_2( N ) = REAL (Returned)
*        Work space.
*     FILE_5( N ) = REAL (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1995 (DSB):
*        Original version.
*     1995 April 6 (MJC):
*        Corrected typo's and made minor stylistic changes.
*        Used the modern-style variable declarations.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER N
      INTEGER NPIX
      INTEGER NLIN
      REAL WLIM
      LOGICAL SNYDER
      REAL FILE_3( N )
      REAL FILE_4( N )
      REAL FILE_8( N )

*  Arguments Given and Returned:
      REAL FILE_1( N )
      REAL FILE_7( N )

*  Arguments Returned:
      REAL FILE_2( N )
      REAL FILE_5( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL D                     ! Observed data value
      INTEGER I                  ! Element count
      REAL S                     ! Simulated data value
      REAL V                     ! Variance value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Form the correction factor for each data value (i.e. the ratio of
*  the real data value to the simulated data value).  The correction
*  factors are stored in file 2.  Do it first using the Snyder
*  modification in which the variance is added to both numerator and
*  denominator.
      IF ( SNYDER ) THEN

         DO I = 1, N

            S = FILE_7( I )
            D = FILE_4( I )
            V = FILE_8( I )

            IF ( S .NE. VAL__BADR .AND. D .NE. VAL__BADR .AND.
     :          V .NE. VAL__BADR ) THEN
               FILE_2 ( I ) = ( D + V ) / ( S + V )

            ELSE
               FILE_2 ( I ) = VAL__BADR

            END IF

         END DO

*  Now do it without the Snyder modification.
      ELSE

         DO I = 1, N

            S = FILE_7( I )
            D = FILE_4( I )

            IF ( S .NE. VAL__BADR .AND. D .NE. VAL__BADR .AND.
     :          S .NE. 0.0 ) THEN
               FILE_2 ( I ) = D / S

            ELSE
               FILE_2 ( I ) = VAL__BADR

            END IF

         END DO

      END IF

*  Convolve the data correction factors with the transposed PSF.  Store
*  the results back in internal file <2>.  This is an image of the mean
*  correction factor at each image pixel.
      CALL KPS1_LUCSM( N, NPIX, NLIN, WLIM, .TRUE., FILE_3, FILE_2,
     :                 FILE_5, FILE_7, STATUS )

*  Multiply the current reconstruction by the mean correction factors
*  to get the next estimate of the reconstructed image.
      DO I = 1, N

         IF ( FILE_1( I ) .NE. VAL__BADR .AND.
     :        FILE_2( I ) .NE. VAL__BADR ) THEN

            FILE_1( I ) = FILE_1( I ) * FILE_2( I )

         ELSE

            FILE_1( I ) = VAL__BADR

         ENDIF

      END DO

      END
