      SUBROUTINE IMG_CANCL( PARAM, STATUS )
*+
*  Name:
*     IMG_CANCL

*  Purpose:
*     Cancels an image/parameter association.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_CANCL( PARAM, STATUS )

*  Description:
*     This routine may be used to release the resources associated with
*     an image. Its behaviour is similar to that of IMG_FREE, except
*     that it also removes the association between an image and a
*     parameter. If the same parameter is later used to access an
*     image, then a new image will be obtained (whereas if IMG_FREE is
*     used the original image will be re-accessed).

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name, specifying either the individual
*        image/parameter association to be cancelled, or '*',
*        indicating that all such associations should be cancelled.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine attempts to execute even if STATUS is set on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.
*
*     - This routine can also be used to cancel multiple images using
*     multiple parameter names. Multiple parameter names are specified
*     using a comma separated list of names (i.e. 'IMAGE1,IMAGE2'), or
*     by using "wild-card" characters as part of a parameter name. In
*     this context, a '*' will match any set of characters, while '%'
*     will match any single character.  Note that only those parameter
*     names previously used to access images via IMG routines are
*     considered as potential matches.
*
*     - Identifiers obtained using IMG_INDF are not released by this
*     routine.

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
*     21-FEB-1992 (RFWS):
*        Original version.
*     15-JUL-1994 (PDRAPER):
*        Changed PARAM to PARAM( F: L) in call in IMG1_VPAR. This
*        corrects a problem with comma separated lists.
*     27-JUL-1994 (PDRAPER):
*        Added code to also release any extension resources.
*     19-AUG-1994 (PDRAPER):
*        Added note about multiple parameter names.
*     13-SEP-1994 (PDRAPER):
*        Re-introduced wild-card matching of parameter names (from
*        RFWS's original code).
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

      INCLUDE 'IMG_PCB'          ! IMG_ Parameter Control Block
*        PCB_PARAM( IMG__MXPAR ) = CHARACTER * ( IMG__SZPAR ) (Read)
*           Parameter name.

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks
      EXTERNAL CHR_WILD
      LOGICAL CHR_WILD           ! Wildcard parameter names match

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      CHARACTER * ( IMG__SZPAR ) MATCH ! Wildcard match string (junk)
      INTEGER ESLOT              ! Extension slot number
      INTEGER F                  ! Position of first character
      INTEGER I1                 ! Position of start of field
      INTEGER I2                 ! Position of end of field
      INTEGER IEND               ! Position of last valid character for
                                 ! wildcard match
      INTEGER L                  ! Position of last character
      INTEGER NPAR               ! Number of non-blank parameter names
      INTEGER SLOT               ! PCB slot number
      LOGICAL OK                 ! Match is positive
      LOGICAL PERC               ! Wildcard match has % sign
      LOGICAL STAR               ! Wildcard match has * sign
      LOGICAL WASNEW             ! New slot? (junk argument)
*.

*  Begin a new error reporting context.
      CALL ERR_BEGIN( STATUS )

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

*  Increment the parameter count and begin a new error reporting
*  context.
               NPAR = NPAR + 1
               CALL ERR_BEGIN( STATUS )

*  If a wild-card parameter specification has been given, then loop
*  through all the parameter slots.
               STAR = INDEX( PARAM( F : L ), '*' ) .NE. 0
               PERC = INDEX( PARAM( F : L ), '%' ) .NE. 0
               IF (  STAR .OR. PERC ) THEN

*  Copy param(f:l) into vpar as need all strings to be same length
*  for chr_wild when using for single character matches, it also
*  appears that no trailing blanks are allowed for '*' !
                  VPAR = PARAM( F : L )
                  IF ( STAR ) THEN
                     IEND = L - F + 1
                  ELSE
                     IEND = IMG__SZPAR
                  END IF
                  DO 2 SLOT = 1, IMG__MXPAR

*  Free all slots which have a parameter association which matches the
*  specification supplied.
                     IF ( ( PCB_PARAM( SLOT ) .NE. ' ' ) ) THEN
                        OK = CHR_WILD( PCB_PARAM( SLOT ), VPAR( :IEND ),
     :                                 MATCH )
                        IF ( OK ) THEN

*  First release any extension resources.
                           DO 3 ESLOT = 1, IMG__MXEXT
                              IF ( ECB_XNAME( SLOT, ESLOT ) .NE. ' ' )
     :                        THEN
                                 CALL IMG1_FREXT( SLOT, ESLOT, STATUS )
                              END IF
 3                         CONTINUE

*  Now free the parameter association.
                           CALL IMG1_CNSLT( SLOT, STATUS )
                        END IF
                     END IF
 2                CONTINUE

*  If any other parameter name was given, validate it and find the slot
*  associated with it.
               ELSE
                  CALL IMG1_VPAR( PARAM( F: L ), VPAR, STATUS )
                  CALL IMG1_GTSLT( VPAR, .FALSE., SLOT, WASNEW, STATUS )

*  Free any extension resources.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     DO 4 ESLOT = 1, IMG__MXEXT
                        IF ( ECB_XNAME( SLOT, ESLOT ) .NE. ' ' ) THEN
                           CALL IMG1_FREXT( SLOT, ESLOT, STATUS )
                        END IF
 4                   CONTINUE

*  Cancel the slot.
                     CALL IMG1_CNSLT( SLOT, STATUS )
                  END IF
               END IF

*  End the error reporting context for the current parameter name.
               CALL ERR_END( STATUS )
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
         CALL ERR_REP( 'IMG_CANCL_NOPAR',
     :                 'No parameter name specified (possible ' //
     :                 'programming error).', STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IMG_CANCL_ERR',
     :   'IMG_CANCL: Error cancelling an image/parameter association.',
     :   STATUS )
      END IF

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
* $Id$
