      SUBROUTINE IMG_DELET( PARAM, STATUS )
*+
*  Name:
*     IMG_DELET

*  Purpose:
*     Deletes an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_DELET( PARAM, STATUS )

*  Description:
*     This routine deletes an image and frees any resources associated
*     with it. It should be used as an alternative to IMG_FREE for any
*     image which is not to be kept.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name specifying the image to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine can also be used to delete multiple images using
*     multiple parameter names. Multiple parameter names are specified
*     using a comma separated list of names (i.e. 'IMAGE1,IMAGE2'). A
*     wildcard capability is not supplied for this routine.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (RFWS):
*        Original version.
*     15-JUL-1994 (PDRAPER):
*        Changed PARAM to PARAM( F: L) in call in IMG1_VPAR. This
*        corrects a problem with comma separated lists.
*     27-JUL-1994 (PDRAPER):
*        Added code to release any extension resources.
*     19-AUG-1994 (PDRAPER):
*        Added note about multiple parameter names.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Global Variables:
      INCLUDE 'IMG_ECB'          ! IMG Extension Control Block
*        ECB_XNAME( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER * ( NDF__SZXNM ) (Read)
*        The name of the extension

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      INTEGER ESLOT              ! Extension slot number
      INTEGER F                  ! Position of first character
      INTEGER I1                 ! Position of start of field
      INTEGER I2                 ! Position of end of field
      INTEGER L                  ! Position of last character
      INTEGER NPAR               ! Number of non-blank parameter names
      INTEGER SLOT               ! PCB slot number
      LOGICAL WASNEW             ! New slot? (junk argument)

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      NPAR = 0

*  Initialise the character pointer to the start of the parameter list.
*  Then loop to extract each element from the parameter list.
      I1 = 1
 1    CONTINUE                   ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( I1 .LE. LEN( PARAM ) ) ) THEN

*  Find the final character of the next element in the parameter list
*  (the last character before a comma or end of string).
         I2 = INDEX( PARAM( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( PARAM )
         ELSE
            I2 = I2 + I1 - 2
         END IF
         IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
            CALL CHR_FANDL( PARAM( I1 : I2 ), F, L )
            IF ( L .GE. F ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Increment the parameter count.
               NPAR = NPAR + 1

*  Validate the parameter name and find the slot associated with it.
               CALL IMG1_VPAR( PARAM( F: L ), VPAR, STATUS )
               CALL IMG1_GTSLT( VPAR, .FALSE., SLOT, WASNEW, STATUS )

*  First release any extension resources.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DO 2 ESLOT = 1, IMG__MXEXT
                     IF ( ECB_XNAME( SLOT, ESLOT ) .NE. ' ' ) THEN
                        CALL IMG1_FREXT( SLOT, ESLOT, STATUS )
                     END IF
 2                CONTINUE

*  Free the slot, deleting the associated NDF.
                  CALL IMG1_FRSLT( SLOT, .FALSE., STATUS )
               END IF
            END IF
         END IF

*  Increment the character pointer to the start of the next element in
*  the parameter list and return to process the next element.
         I1 = I2 + 2
         GO TO 1
      END IF

*  If no error has occurred, but no non-blank parameter names have been
*  processed, then report an error.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NPAR .EQ. 0 ) ) THEN
         STATUS = IMG__PARIN
         CALL ERR_REP( 'IMG_DELET_NOPAR',
     :                 'No parameter name specified (possible ' //
     :                 'programming error).', STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IMG_DELET_ERR',
     :   'IMG_DELET: Error deleting an image.', STATUS )
      END IF

      END
* $Id$
