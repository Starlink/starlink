      SUBROUTINE KPS1_MEMOU( FILE1, FILE22, NPIX, NLIN, MODEL, NPIXM,
     :                       NLINM, XMARG, YMARG, OUT, BAD, STATUS )
*+
*  Name:
*     KPS1_MEMOU

*  Purpose:
*     Copy the final MEM image from the internal file <1> to an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMOU( FILE1, FILE22, NPIX, NLIN, MODEL, NPIXM, NLINM,
*                      XMARG, YMARG, OUT, BAD, STATUS )

*  Description:
*     The area of file 1 corresponding to the output image (i.e.
*     excluding the blank margins) is copied to the output. Any pixels
*     with zero accuracy or zero model value are set invalid in the
*     output.

*  Arguments:
*     FILE1( C1_NPX, C1_NLN ) = REAL (Given)
*        Internal file <1> holding the reconstruction image.
*     FILE22( C1_NPX, C1_NLN ) = REAL (Given)
*        Internal file <22> holding the data accuracies.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in the output image.
*     NLIN = INTEGER (Given)
*        The number of lines in the output image.
*     MODEL( NPIXM, NLINM ) = REAL (Given)
*        The default model. If the array has dimensions (1,1) then the
*        single value is used for all pixels.
*     NPIXM = INTEGER (Given)
*        The number of pixels per line in the default model image.
*     NLINM = INTEGER (Given)
*        The number of lines in the default model image.
*     XMARG = INTEGER (Given)
*        The width of the left hand X margin, in pixels.
*     YMARG = INTEGER (Given)
*        The width of the bottom Y margin, in pixels.
*     OUT( NPIX, NLIN ) = REAL (Returned)
*        The output image.
*     BAD = LOGICAL (Returned)
*        True is any bad pixels were set in the output. False otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
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
*     28-SEP-1990 (DSB):
*        Original version.
*     25-FEB-1991 (DSB):
*        Variable model allowed for.
*      4-MAR-1991 (DSB):
*        Name changed from SETOUT to KPS1_SETOU.
*     1991 July 17 (MJC):
*        Name changed from KPS1_SETOU to KPS1_MEMOU.
*     {enter_further_changes_here}

*-


*  Type Definitions:

      IMPLICIT NONE              ! No implicit typing


*  Global Constants:

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Data constants


*  Global Variables:

      INCLUDE 'C1_COM'           ! Used to communicate with OPUS and
                                 ! TROPUS.
*        C1_NPX = INTEGER (Read)
*           The X dimension of all internal images (including margin).
*        C1_NLN = INTEGER (Read)
*           The Y dimension of all internal images (including margin).


*  Arguments Given:

      INTEGER  NPIXM
      INTEGER  NLINM
      REAL     MODEL( NPIXM, NLINM )
      REAL     FILE1( C1_NPX, C1_NLN )
      REAL     FILE22( C1_NPX, C1_NLN )
      INTEGER  NPIX
      INTEGER  NLIN
      INTEGER  XMARG
      INTEGER  YMARG


*  Arguments Returned:

      REAL     OUT( NPIX, NLIN )
      LOGICAL  BAD


*  Status:

      INTEGER STATUS             ! Global status


*  Local Variables:

      LOGICAL  GOOD              ! True if any good pixels found.
      INTEGER  LIN               ! Line counter.
      INTEGER  PIX               ! Pixel counter.

*.


*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN


*  Initialise BAD and GOOD to indicate that there are no bad or good
*  pixels in the output.

      BAD = .FALSE.
      GOOD = .FALSE.


*  Loop round the output image, assuming  a constant non-zero model
*  value...

      IF( NPIXM*NLINM .EQ. 1 ) THEN

         DO LIN = 1, NLIN
            DO PIX = 1, NPIX


*  If the corresponding accuracy value is not positive, set the output
*  invalid.

               IF( FILE22( PIX + XMARG, LIN + YMARG ) .GT. 0.0 ) THEN
                  OUT( PIX, LIN ) = FILE1( PIX + XMARG, LIN + YMARG )
                  GOOD = .TRUE.

               ELSE
                  OUT( PIX, LIN ) = VAL__BADR
                  BAD = .TRUE.

               END IF

            END DO

         END DO


*  Now do the same, assuming  a variable model. Set all pixels with zero
*  model value to the BAD value...

      ELSE

         DO LIN = 1, NLIN
            DO PIX = 1, NPIX


*  If the corresponding accuracy value is not positive, set the output
*  invalid.

               IF( FILE22( PIX + XMARG, LIN + YMARG ) .LE. 0.0 ) THEN
                  OUT( PIX, LIN ) = VAL__BADR
                  BAD = .TRUE.


*  If the corresponding model value is not positive, set the output
*  invalid.

               ELSE IF( MODEL( PIX + XMARG, LIN + YMARG ) .LE. 0.0 )
     :                                                              THEN
                  OUT( PIX, LIN ) = VAL__BADR
                  BAD = .TRUE.


*  Otherwise, read the output value from file <1>.

               ELSE
                  OUT( PIX, LIN ) = FILE1( PIX + XMARG, LIN + YMARG )
                  GOOD = .TRUE.

               END IF

            END DO

         END DO

      END IF

*  If there were no good pixels in the output, abort.

      IF( .NOT. GOOD ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MEMOU_ERR1',
     :             'The output image contains no good pixels.', STATUS )
      END IF


*  Finish.

      END
