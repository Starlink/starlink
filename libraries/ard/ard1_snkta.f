      SUBROUTINE ARD1_SNKTA( STATUS )
*+
*  Name:
*     ARD1_SNKTA

*  Purpose:
*     Write AST_ data as text to a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_SNKTA( STATUS )

*  Description:
*     This is a service routine to be provided as a "sink" routine for
*     the AST_CHANNEL function. It takes data in the form of text (in
*     response to writing an AST_ object to a Channel) and delivers it
*     to a GRP group for storage.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-AUG-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'ARD_ERR'          ! ARD error constants
      INCLUDE 'ARD_CONST'        ! ARD private constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_AGRP = INTEGER (Read)
*           GRP identifier for group holding AST_ data.
*        CMN_ASTLN = INTEGER (Read and Write)
*           Next element to use in the group holding AST_ data.

*  Global Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER SZTEXT             ! Size of text buffer
      PARAMETER ( SZTEXT = ( GRP__SZNAM - 1 ) * ( ARD__MXACL + 1 ) )

      INTEGER ASTTSZ             ! Max. length of text which can be stored
      PARAMETER ( ASTTSZ = GRP__SZNAM - 4 ) ! in a single element

*  Local Variables:
      CHARACTER FLAG*1           ! Flag character
      CHARACTER TEXT*( SZTEXT + 1 ) ! Buffer for AST_ text
      INTEGER I1                 ! Index of first character to write
      INTEGER I2                 ! Index of last character to write
      INTEGER L                  ! Number of characters in AST_ text
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the text to be written.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL AST_GETLINE( TEXT, L, STATUS )

*  If the text was obtained successfully, then check its length. If
*  this exceeds what can safely be handled, report an error.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( L .GT. SZTEXT ) ) THEN
            STATUS = ARD__BADAR
            CALL MSG_SETI( 'ACL', ARD__MXACL )
            CALL ERR_REP( 'ARD1_SNKTA_XS', 'Maximum number of '//
     :                    'continuation lines (^ACL) exceeded - '//
     :                    'output text is too long.', STATUS )
         END IF
      END IF

*  Remove any leading and trailing blanks (this removes any indentation).
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( L .GT. 0 ) THEN
            CALL CHR_LDBLK( TEXT( : L ) )
            L = CHR_LEN( TEXT( : L ) )
         END IF

*  Initialise the flag character for continuation lines.
         IF ( L .GT. 0 ) THEN
            FLAG = ' '

*  Loop to write the text into the group as a sequence of lines of length
*  one less than the max. number of characters in each group element (the
*  first character of each line is reserved for use as a flag character).
            DO 1 I1 = 1, L, ASTTSZ - 1

*  Find the last character to be included in the current line.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  I2 = MIN( I1 + ASTTSZ - 2, L )

*  Write the line, prefixed by the flag character.
                  CALL GRP_PUT( CMN_AGRP, 1, FLAG // TEXT( I1 : I2 ),
     :                          CMN_ASTLN, STATUS )

*  Increment the line number to be used next, and note that the next
*  line (if any) will be a continuation line.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CMN_ASTLN = CMN_ASTLN + 1
                     FLAG = '+'
                  END IF
               END IF

*  Quit looping if an error occurs.
               IF ( STATUS .NE. SAI__OK ) GO TO 2
 1          CONTINUE
 2          CONTINUE
         END IF
      END IF

      END
