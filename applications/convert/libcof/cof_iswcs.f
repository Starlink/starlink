      LOGICAL FUNCTION COF_ISWCS( INDF, STATUS )
*+
*  Name:
*     COF_ISWCS

*  Purpose:
*     See if the FITS extension of an NDF contains WCS information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = COF_ISWCS( INDF, STATUS )

*  Description:
*     This routine attempts to read a FrameSet from the FITS extension of
*     the supplied NDF. If succesfull, and if the FrameSet has more than
*     one Frame (the pixel grid), then .TRUE. is returned. NATIVE
*     encodings in the FITS extension are ignored.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     .TRUE. if the FITS extension of the NDF has usable WCS information.

*  Copyright:
*     Copyright (C) 2003 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-FEB-2003 (DSB):
*        Original version.
*     9-JUN-2003 (DSB):
*        Corrected logic for breaking out of object reading loop.
*     20-MAY-2013 (DSB):
*        Flush error and continue if an invalid FITS card is read.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'AST_ERR'          ! AST error constants
      INCLUDE 'DAT_PAR'          ! HDS constants

*  Arguments Given:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CARD*85          ! FITS header card
      CHARACTER CLOC*(DAT__SZLOC)! Locator for FITS header card
      CHARACTER ENC*30           ! Current FitsChan encoding
      CHARACTER LOC*(DAT__SZLOC) ! Locator for FITS extension
      INTEGER FC                 ! AST FitsChan identifier
      INTEGER ICARD              ! Index of curent header cards
      INTEGER NCARD              ! No. of header cards in the FITS extension
      INTEGER OBJ                ! AST Object read from the FITS extension
      LOGICAL MORE               ! Read another Object?
      LOGICAL THERE              ! Does object exist?
*.

*  Initialise.
      COF_ISWCS = .FALSE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  See if there is a FITS extension in the NDF.
      CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )

*  If so...
      IF( THERE ) THEN

*  Create a FitsChan.
         FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )

*  Find the FITS extension, and get its size.
         CALL NDF_XLOC( INDF, 'FITS', 'READ', LOC, STATUS )
         CALL DAT_SIZE( LOC, NCARD, STATUS )

*  Loop round putting each card into the FitsChan. If an invalid card is
*  read, flush the error and continue in the hope that omitting the card
*  will have no effect on the WCS.
         DO ICARD = 1, NCARD
            CALL DAT_CELL( LOC, 1, ICARD, CLOC, STATUS )
            CALL DAT_GET0C( CLOC, CARD, STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL AST_PUTFITS( FC, CARD, .FALSE., STATUS )
               IF( STATUS .EQ. AST__BDFTS ) THEN
                  CALL ERR_REP( ' ', 'Attempting to continuing...',
     :                          STATUS )
                  CALL ERR_FLUSH( STATUS )
               END IF
            ENDIF

            CALL DAT_ANNUL( CLOC, STATUS )
         END DO

*  Annul the locator to the FITS extensuion array.
         CALL DAT_ANNUL( LOC, STATUS )

*  Loop, attempting to read a FrameSet from the FitsChan, until a foregn
*  encoding with more than 1 Frame is read.
         MORE = .TRUE.
         DO WHILE( MORE )
            ENC = AST_GETC( FC, 'ENCODING', STATUS )
            CALL AST_CLEAR( FC, 'CARD', STATUS )
            OBJ = AST_READ( FC, STATUS )

*  Leave the loop if no more objects can be read.
            IF( OBJ .EQ. AST__NULL ) THEN
               MORE = .FALSE.

*  If an non-NATIVE object was read, see if it is a FrameSet containing more
*  than 1 Frame.
            ELSE IF( ENC .NE. 'NATIVE' ) THEN
               IF( AST_ISAFRAMESET( OBJ, STATUS ) ) THEN
                  IF( AST_GETI( OBJ, 'NFRAME', STATUS ) .GT. 1 ) THEN
                     COF_ISWCS = .TRUE.
                     MORE = .FALSE.
                  END IF
               END IF
            END IF
         END DO
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
