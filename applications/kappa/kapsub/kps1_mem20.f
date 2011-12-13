      SUBROUTINE KPS1_MEM20( MODEL, NPIX, NLIN, FILE20, STATUS )
*+
*  Name:
*     KPS1_MEM20

*  Purpose:
*     Stores the given default model image in internal file <20> for
*     a maximum-entropy reconstruction.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEM20( MODEL, NPIX, NLIN, FILE20, STATUS )

*  Description:
*     The supplied image is padded with zero-filled margins to make it
*     the same size as the deconvolved image. Any bad pixels are replace
*     by the value zero. Any zero or negative pixels in the input image
*     cause the program to abort.

*  Arguments:
*     MODEL( NPIX, NLIN ) = REAL (Given)
*        The model image supplied by the user.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in the model. This must be the
*        same as that of the original image which was given as input to
*        the deconvolution process.
*     NLIN = INTEGER (Given)
*        The number of lines in the model. This must be the same as
*        that of the original image which was given as input to the
*        deconvolution process.
*     FILE20( C1_NPX, C1_NLN ) = REAL (Returned)
*        Internal file <20>, containing the model after replacement of
*        bad pixels, and padding.
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
*     18-OCT-1990 (DSB):
*        Original version.
*      4-MAY-1991 (DSB):
*        Name changed from SET20 to KPS1_SET20.
*     1991 July 18 (MJC):
*        Name changed from KPS1_SET20 to KPS$MEM20.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Data constants

*  Global Variables:
      INCLUDE 'ME_COM'           ! MEMSYS3 common blocks
      INCLUDE 'C1_COM'           ! MEM2D internal communication.

*  Arguments Given:
      INTEGER NPIX
      INTEGER NLIN
      REAL    MODEL( NPIX, NLIN )

*  Arguments Returned:
      REAL    FILE20( C1_NPX, C1_NLN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER  LIN               ! Line counter
      REAL     MODVAL            ! Current model value
      INTEGER  NGOOD             ! No. of good pixels in the model
      INTEGER  PIX               ! Pixel counter.
*.


*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN


*  Fill file <20> with zeros.

      DO LIN = 1, C1_NLN
         DO PIX = 1, C1_NPX
            FILE20( PIX, LIN ) = 0.0
         END DO
      END DO


*  Copy the model to file <20> incorporating the blank margins. Bad
*  pixels are left at the zero value set up above.

      NGOOD = 0

      DO LIN  = 1, NLIN

         DO PIX = 1, NPIX
            MODVAL = MODEL( PIX, LIN )



            IF( MODVAL .NE. VAL__BADR ) THEN

*  If the model value is not positive, abort.

               IF( MODVAL .LE. 0.0 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'KPS1_MEM20_ERR1',
     :               'Model pixels with negative or zero values found.',
     :                           STATUS )
                  GO TO 999


*  If the data value is good and positive, store it and increment
*  statistics.

               ELSE
                  FILE20( C1_XMG + PIX, C1_YMG + LIN ) = MODVAL
                  NGOOD = NGOOD + 1

               ENDIF

            END IF

         END DO

      END DO


*  If the model has no good pixels, abort.

      IF( NGOOD .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_MEM20_ERR1',
     :                 'No usable pixels found in model image.',
     :                  STATUS )
         GOTO 999
      END IF


*  Finish.

 999  CONTINUE

      END
