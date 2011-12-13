      SUBROUTINE HDR_MOD( PARAM, STATUS )
*+
*  Name:
*    HDR_MOD

*  Purpose:
*     Opens an image allowing modification of any header items.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDR_MOD( PARAM, STATUS )

*  Description:
*     This subroutine opens an image and allows the header information
*     to be modified by other HDR routines.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the image (case insensitive).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine may be used to obtain modification access to more
*     than one image at a time by using multiple parameter
*     names. Multiple parameter names are provided as a comma separated
*     list (i.e. 'IN1,IN2,IN3'). Note that the argument VALUE must be
*     declared as a dimension of size at least the number of parameters
*     in the list, if this option is used.

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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     1-DEC-1995 (PDRAPER):
*        Original version.
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
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Global Variables:
      INCLUDE 'IMG_PCB'          ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      INTEGER F                  ! First character position
      INTEGER I1                 ! Position of start of field
      INTEGER I2                 ! Position of end of field
      INTEGER L                  ! Last character positiong
      INTEGER NPAR               ! Number of parameters
      INTEGER SLOT               ! Parameter slot number
      LOGICAL CANMOD             ! Can modify NDF
      LOGICAL WASNEW             ! Dummy
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the parameter count.
      NPAR = 0

*  Initialise the character pointer to the start of the parameter list.
*  Then loop to extract each element from the list.
      I1 = 1
 1    CONTINUE                   ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( I1 .LE. LEN( PARAM ) ) )
     :     THEN

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

*  Validate the parameter and its slot number.
               CALL IMG1_VPAR( PARAM( F: L ), VPAR, STATUS )
               CALL IMG1_GTSLT( VPAR, .TRUE., SLOT, WASNEW, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If a new parameter slot was allocated then we need to access an NDF.
*  The NDF data is not mapped in this case for efficiency reasons.
                  IF ( WASNEW ) CALL IMG1_ASSOC( VPAR, 'UPDATE', SLOT,
     :                                           STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that WRITE access can be used on this extension.
                     CALL NDF_ISACC( PCB_INDF( SLOT ), 'WRITE', CANMOD,
     :                               STATUS )
                     IF ( .NOT. CANMOD ) THEN

*  Cannot write to this NDF's extension.
                        STATUS = IMG__NOACC
                        CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
                        IF ( WASNEW ) THEN
                           CALL ERR_REP( ' ', 'Cannot open the image '//
     :                          '''^NDF'' for modification '//
     :                          '(write access is not allowed)',
     :                          STATUS )
                        ELSE
                           CALL ERR_REP( ' ', 'The image ''^NDF'' is '//
     :                          'already opened for readonly access '//
     :                          'and consequently its header '//
     :                          'information cannot be modified',
     :                          STATUS )
                        END IF
                     END IF
                  END IF
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
         CALL ERR_REP( 'HDR_MOD_NOPAR',
     :        'No parameter name specified (possible ' //
     :        'programming error).', STATUS )
      END IF
      END
* @(#)hdr_mod.f   1.4   95/12/04 11:38:28   96/05/17 14:24:22
