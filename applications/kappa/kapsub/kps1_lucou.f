      SUBROUTINE KPS1_LUCOU( NPIX, NLIN, FILE_1, XMARG, YMARG, NP, NL,
     :                       OUT, BAD, STATUS )
*+
*  Name:
*     KPS1_LUCOU

*  Purpose:
*     Copies the final Lucy-restored image from the internal file <1>
*     to an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LUCOU( NPIX, NLIN, FILE_1, XMARG, YMARG, NP, NL, OUT,
*                      BAD, STATUS )

*  Description:
*     The area of file 1 corresponding to the output image (i.e.
*     excluding the margins) is copied to the output, and a flag is
*     returned indicating if any bad values were found.

*  Arguments:
*     NPIX = INTEGER (Given)
*        The number of pixels per line in each internal file.
*     NLIN = INTEGER (Given)
*        The number of lines in each internal file.
*     FILE_1( NPIX, NLIN ) = REAL (Given)
*        Internal file <1> holding the reconstructed image.
*     XMARG = INTEGER (Given)
*        The width of the left-hand x margin, in pixels.
*     YMARG = INTEGER (Given)
*        The width of the bottom y margin, in pixels.
*     NP = INTEGER (Given)
*        The number of pixels per line in the output image.
*     NL = INTEGER (Given)
*        The number of lines in the output image.
*     OUT( NP, NL ) = REAL (Returned)
*        The output image.
*     BAD = LOGICAL (Returned)
*        It is .TRUE. if any bad values were stored in the output.  It
*        is .FALSE. otherwise.
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
*     28-FEB-1995 (DSB):
*        Original version.
*     1995 April 6 (MJC):
*        Minor stylistic changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER  NPIX
      INTEGER  NLIN
      REAL     FILE_1( NPIX, NLIN )
      INTEGER  XMARG
      INTEGER  YMARG
      INTEGER  NP
      INTEGER  NL

*  Arguments Returned:
      REAL     OUT( NP, NL )
      LOGICAL  BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL  GOOD              ! True if any good pixels found.
      INTEGER  LIN               ! Line counter.
      INTEGER  PIX               ! Pixel counter.

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise BAD and GOOD to indicate that there are no bad or good
*  pixels in the output.
      BAD = .FALSE.
      GOOD = .FALSE.

*  Loop round the output image.
      DO LIN = 1, NL
         DO PIX = 1, NP

*  Store the reconstructed image value in the output array.
            OUT( PIX, LIN ) = FILE_1( PIX + XMARG, LIN + YMARG )

*  Update the bad pixel flags.
            IF ( OUT( PIX, LIN ) .EQ. VAL__BADR ) THEN
               BAD = .TRUE.
            ELSE
               GOOD = .TRUE.
            END IF

         END DO

      END DO

*  If there were no good pixels in the output, abort.
      IF ( .NOT. GOOD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_LUCOU_ERR1', 'The output image contains '/
     :                 /'no good pixels.', STATUS )
      END IF

      END
