      SUBROUTINE KPS1_LUCCP( XMARG, YMARG, DIM1, DIM2, IN, NPIX, NLIN,
     :                       OUT, MEAN, STATUS )
*+
*  Name:
*     KPS1_LUCCP

*  Purpose:
*     Copies an input aray to a larger output array, and find the mean.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LUCCP( XMARG, YMARG, DIM1, DIM2, IN, NPIX, NLIN, OUT,
*                      MEAN, STATUS )

*  Description:
*     The IN array is copied to the OUT array, leaving a margin of
*     XMARG pixels at the left hand edge, and YMARG pixels at the
*     bottom.  The margins are filled with the nearest edge values.  The
*     mean data value is returned.

*  Arguments:
*     XMARG = INTEGER (Given)
*        The width of the left-hand x-axis margin, in pixels.
*     YMARG = INTEGER (Given)
*        The width of the bottom y-axis margin, in pixels.
*     DIM1 = INTEGER (Given)
*        Number of pixels per line in the input data.
*     DIM2 = INTEGER (Given)
*        Number of lines in the input data.
*     IN( DIM1, DIM2 ) = REAL (Given)
*        The input array.
*     NPIX = INTEGER (Given)
*        Number of pixels per line in the output data.
*     NLIN = INTEGER (Given)
*        Number of lines in the output data.
*     OUT( NPIX, NLIN ) = REAL (Returned)
*        The output array.
*     MEAN = REAL (Returned)
*        The mean value in OUT.  Returned equal to VAL__BADR if there
*        are no good data in the array.
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
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Data constants

*  Arguments Given:
      INTEGER  XMARG
      INTEGER  YMARG
      INTEGER  DIM1
      INTEGER  DIM2
      REAL     IN( DIM1, DIM2 )
      INTEGER  NPIX
      INTEGER  NLIN

*  Arguments Returned:
      REAL     OUT( NPIX, NLIN )
      REAL     MEAN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER  LIN               ! Line counter
      INTEGER  NGOOD             ! Good pixel count
      INTEGER  PIX               ! Pixel counter
      REAL VAL                   ! Edge pixel value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of good values found so far, and the sum of
*  the good values.
      NGOOD = 0
      MEAN = 0.0

*  Copy the input array to the output array leaving the required
*  margins.
      DO LIN  = 1, DIM2
         DO PIX = 1, DIM1
            OUT( XMARG + PIX, YMARG + LIN ) = IN( PIX, LIN )

*  If this is a good value, increment the statistics.
            IF ( IN( PIX, LIN ) .NE. VAL__BADR ) THEN
               MEAN = MEAN + IN( PIX, LIN )
               NGOOD = NGOOD + 1
            END IF

         END DO
      END DO

*  Replicate the edge pixels into the margins.
      DO LIN = YMARG + 1, YMARG + DIM2

         VAL = OUT( XMARG + 1, LIN )
         DO PIX = 1, XMARG
            OUT( PIX, LIN ) = VAL
         END DO

         VAL = OUT( XMARG + DIM1, LIN )
         DO PIX = XMARG + DIM1 + 1, NPIX
            OUT( PIX, LIN ) = VAL
         END DO

      END DO

      DO LIN = YMARG, 1, -1
         DO PIX = 1, NPIX
            OUT( PIX, LIN ) = OUT( PIX, LIN + 1 )
         END DO
      END DO

      DO LIN = YMARG + DIM2 + 1, NLIN
         DO PIX = 1, NPIX
            OUT( PIX, LIN ) = OUT( PIX, LIN - 1 )
         END DO
      END DO

*  Return the mean value (excluding the margins).
      IF ( NGOOD .GT. 0 ) THEN
         MEAN = MEAN / NGOOD
      ELSE
         MEAN = VAL__BADR
      END IF

      END
