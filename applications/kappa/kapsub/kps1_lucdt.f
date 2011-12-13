      SUBROUTINE KPS1_LUCDT( N, NPIX, NLIN, XMARG, YMARG, CHIFAC, WLIM,
     :                       FILE_3, FILE_4, FILE_6, FILE_1, FILE_8,
     :                       FILE_7, FILE_2, FILE_5, XSQ, STATUS )
*+
*  Name:
*     KPS1_LUCDT

*  Purpose:
*     Creates simulated data from the current reconstructed image
*     and return the corresponding normalised chi-squared value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LUCDT( N, NPIX, NLIN, XMARG, YMARG, CHIFAC, WLIM,
*                      FILE_3, FILE_4, FILE_6, FILE_1, FILE_8, FILE_7,
*                      FILE_2, FILE_5, XSQ, STATUS )

*  Description:
*     Simulated data is formed by convolving the current reconstructed
*     image (in file 1) with the PSF and adding the background data.
*     The simulated data is returned in file 7.  The deviation of the
*     simulated data from the observed data is then estimated in terms
*     of a chi-squared value, based on the variances supplied in file 8
*     and the value of CHIFAC.  Edge pixels are excluded from the
*     estimation of chi-squared.

*  Arguments:
*     N = INTEGER (Given)
*        The number of elements in each internal file.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in each internal file.
*     NLIN = INTEGER (Given)
*        The number of lines in each internal file.
*     XMARG = INTEGER (Given)
*        The width of the left- and right-hand margins in pixels.
*     YMARG = INTEGER (Given)
*        The width of the top and bottom margins in pixels.
*     CHIFAC = REAL (Given)
*        The co-efficient of the simulated data in the denominator of
*        the expression used to evaluate the chi-squared of each data
*        value.
*     WLIM = REAL (Given)
*        Weight limit for good pixels.
*     FILE_3( NPIX, NLIN ) = REAL (Given)
*        The Fourier transform of the PSF.
*     FILE_4( NPIX, NLIN ) = REAL (Given)
*        The observed data.
*     FILE_6( NPIX, NLIN ) = REAL (Given)
*        The background data.
*     FILE_1( NPIX, NLIN ) = REAL (Given)
*        The current reconstructed image.
*     FILE_8( NPIX, NLIN ) = REAL (Given)
*        The accuracy of each data value.
*     FILE_7( NPIX, NLIN ) = REAL (Returned)
*        Returned holding the simulated data.
*     FILE_2( NPIX, NLIN ) = REAL (Returned)
*        Work space.  Returned holding the simulated data minus the
*        background data.
*     FILE_5( NPIX, NLIN ) = REAL (Returned)
*        Work space.
*     XSQ = REAL( Returned)
*        The normalised chi-squared value describing the deviation of
*        the simulated data returned in file 7, from the observed data
*        in file 4.  N.B., this is the chi-squared per pixel.
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
      INTEGER XMARG
      INTEGER YMARG
      REAL CHIFAC
      REAL WLIM
      REAL FILE_3( NPIX, NLIN )
      REAL FILE_4( NPIX, NLIN )
      REAL FILE_6( NPIX, NLIN )
      REAL FILE_1( NPIX, NLIN )
      REAL FILE_8( NPIX, NLIN )

*  Arguments Returned:
      REAL FILE_7( NPIX, NLIN )
      REAL FILE_2( NPIX, NLIN )
      REAL FILE_5( NPIX, NLIN )
      REAL XSQ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL B                     ! Background value
      REAL D                     ! Observed data value
      REAL DENOM                 ! Chi-squared denominator
      INTEGER I                  ! Column count
      INTEGER J                  ! Row count
      INTEGER NVAL               ! No. of values summed in XSQ
      REAL S                     ! Simulated data value
      REAL V                     ! Variance value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the current reconstruction from file <1> to file <7>.
      DO J = 1, NLIN
         DO I = 1, NPIX
            FILE_7( I, J ) = FILE_1( I, J )
         END DO
      END DO

*  Convolve file 7 (i.e. the current reconstruction) with the PSF.
      CALL KPS1_LUCSM( N, NPIX, NLIN, WLIM, .FALSE., FILE_3,
     :                 FILE_7, FILE_5, FILE_2, STATUS )

*  Add the background data back on to get simulated data values.
      DO J = 1, NLIN
         DO I = 1, NPIX

            S = FILE_7( I, J )
            B = FILE_6( I, J )

            IF ( S .NE. VAL__BADR .AND. B .NE. VAL__BADR ) THEN
               FILE_7( I, J ) = S + B

            ELSE
               FILE_7( I, J ) = VAL__BADR

            END IF

         END DO
      END DO

*  Initialise the chi-squared value, and the number of values which
*  have contributed to it.
      XSQ = 0.0
      NVAL = 0

*  Form the chi-squared value, excluding the margins.
      DO J = 1 + YMARG, NLIN - YMARG
         DO I = 1 + XMARG, NPIX - XMARG

            S = FILE_7( I, J )
            V = FILE_8( I, J )
            D = FILE_4( I, J )

            IF ( S .NE. VAL__BADR .AND. V.NE. VAL__BADR .AND.
     :           D .NE. VAL__BADR ) THEN

               DENOM = V + CHIFAC * ABS( S )

               IF ( DENOM .GT. 0.0 ) THEN
                  XSQ = XSQ + ( ( ABS( S - D ) ) **2 ) / DENOM
                  NVAL = NVAL + 1
               END IF

            END IF

         END DO
      END DO

*  Return the normalised chi-squared.
      IF ( NVAL .GT. 0 ) THEN
         XSQ = XSQ / NVAL

      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_LUCDT_ERR1', 'No good data are left. '/
     :                 /'(Is parameter WLIM too high?)', STATUS )

      END IF

      END
