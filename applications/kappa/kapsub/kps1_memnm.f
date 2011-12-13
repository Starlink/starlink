      SUBROUTINE KPS1_MEMNM( DEF, NPIX, NLIN, FILE1, FILE20, FILE22,
     :                       FILE2, OUT, STATUS )
*+
*  Name:
*     KPS1_MEMNM

*  Purpose:
*     Creates a new default model image in a maximum-entropy
*     reconstruction.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMNM( DEF, NPIX, NLIN, FILE1, FILE20, FILE22, FILE2,
*                      OUT, STATUS )

*  Description:
*     The new model is a linear combination of the old model and the
*     current hidden reconstruction. The proportions of each of these
*     two images in the new model varies from pixel to pixel.  Pixels
*     which have a high entropy in the current hidden reconstruction are
*     determined by the current hidden reconstruction, where as pixels
*     with a low entropy are determined by the old model.

*  Arguments:
*     DEF = REAL (Given)
*        The value used for the MEM3 argument DEF.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in the output image.
*     NLIN = INTEGER (Given)
*        The number of lines in the output image.
*     FILE1( * ) = REAL (Given)
*        File <1> - the current reconstruction.
*     FILE20( * ) = REAL (Given)
*        File <20> - the current model.
*     FILE22( * ) = REAL (Given)
*        File <22> - the data accuracies.
*     FILE2( * ) = REAL (Returned)
*        Used as work space.
*     OUT( NPIX, NLIN ) = REAL (Returned)
*        The output image to hold the new model.
*     STATUS = INTEGER (Given and Returned)
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1991 (DSB):
*        Original version.
*     28-FEB-1991 (DSB):
*        Name changed from NEWMOD to KPS1_NEWMO.
*     1991 July 18 (MJC):
*        Name changed from KPS1_NEWMO to KPS1_MEMNM.  Calls to
*        KPS1_SETOU altered to the new name KPS1_MEMOU.
*     20-MAR-1995 (DSB):
*        Modified to allow use of external work arrays.
*     1995 April 7 (MJC):
*        Minor stylistic changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'C1_COM'           ! Used to communicate with OPUS and
                                 ! TROPUS.
      INCLUDE 'ME_COM'           ! Common blocks etc required by MEMSYS3

*  Arguments Given:
      REAL DEF
      INTEGER NPIX
      INTEGER NLIN
      REAL FILE1( * )
      REAL FILE20( * )
      REAL FILE22( * )

*  Arguments Returned:
      REAL FILE2( * )
      REAL OUT( NPIX, NLIN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL   BAD              ! True if the output image contains
                                 ! any bad pixels
      REAL	F                ! Reconstruction pixel value
      REAL	GAIN             ! A speed at which the model is
                                 ! allowed to change
      INTEGER	I                ! Index in image
      REAL	M                ! Model pixel value
      INTEGER	NUSED            ! No. of good entropy values found
      REAL	S                ! Entropy pixel value
      REAL	SMAX             ! Maximum entropy per pixel
      REAL	SMEAN            ! Mean entropy per pixel
      REAL	STOT             ! Total entropy

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a constant default model was used in the current reconstruction
*  (indicated by ME_KB(20) being zero), then store the constant value.
*  This value will be overwritten by the variable default model values
*  if a variable default model is being used.
      M = DEF

*  Create the (minus) entropy image in file <2>.
      SMAX = 0.0
      STOT = 0.0
      NUSED = 0

      DO I = 1, C1_NPX * C1_NLN

         F = FILE1( I )
         IF ( ME_KB( 20 ) .GT. 0 ) M = FILE20( I )

         IF ( F .GT. 0 .AND. M .GT. 0 ) THEN
            S =  - F + M + F * LOG( F / M )
            NUSED = NUSED + 1

         ELSE
            S = 0.0

         END IF

         FILE2( I ) = S
         SMAX = MAX( SMAX, S )
         STOT = STOT + S

      END DO

*  If the total entropy is zero, then the model is left as it is.
      IF ( STOT .GT. 0.0 ) THEN

*  Calculate a factor which causes the model to change more slowly if
*  the entropy of all pixels are similar.
         SMEAN = STOT / REAL( NUSED )
         GAIN = MAX( 1.0 / REAL( NUSED ), 1.0 - SMEAN / SMAX )

*  Form the new model in file <2>.
         DO I = 1, C1_NPX * C1_NLN

            F = FILE1( I )
            IF ( ME_KB( 20 ) .GT. 0 ) M = FILE20( I )
            S = FILE2( I )
            FILE2( I ) = M + GAIN * ( F - M ) * S / SMAX

         END DO

      END IF

*  Copy the new model to the output NDF.
      IF ( ME_KB( 20 ) .EQ. 0 ) THEN
         CALL KPS1_MEMOU( FILE2, FILE22, NPIX, NLIN, DEF, 1, 1, C1_XMG,
     :                    C1_YMG, OUT, BAD, STATUS )

      ELSE
         CALL KPS1_MEMOU( FILE2, FILE22, NPIX, NLIN, FILE20, C1_NPX,
     :                    C1_NLN, C1_XMG, C1_YMG, OUT, BAD, STATUS )

      END IF

      END
