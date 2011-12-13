      SUBROUTINE KPG1_SNKTA( STATUS )
*+
*  Name:
*     KPG1_SNKTA

*  Purpose:
*     Writes AST_ data as text to a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SNKTA( STATUS )

*  Description:
*     This is a service routine to be provided as a "sink" routine for
*     the AST_CHANNEL function. It takes data in the form of text (in
*     response to writing an AST_ object to a Channel) and delivers it
*     to a GRP group for storage.
*
*     This routine has only a STATUS argument, so it communicates with
*     other KPG routines via global variables stored in the KPG_AST
*     common blocks. These are described below under "Global Variables
*     used as Arguments".

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Global Variables used as Arguments:
*     ASTGRP = INTEGER (Given)
*        A GRP identifier for the group which is to store the data.
*     ASTTSZ = INTEGER (Given)
*        The maximum length of text which should be stored in a single
*        element of the group. This should be less than or equal to
*        GRP__SZNAM.
*     ASTLN = INTEGER (Given and Returned)
*        This must initially be set to the value 1, to indicate that
*        data will be written starting at the first element of the
*        group (note the routine will not operate correctly unless 1 is
*        the initial value - you cannot start writing at another point
*        in the group if you have previously written to a different
*        group). On exit it will be incremented by the number of
*        elements used to store data, so that it identifies the first
*        element to be used on the next invocation.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-FEB-1998 (DSB):
*        Original version, based on NDF1_WRAST.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants (needed by KPG_AST)
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'KPG_PAR'          ! KPG_ constants

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks
*        ASTGRP = INTEGER (Read)
*           GRP identifier for group holding AST_ data.
*        ASTTSZ = INTEGER (Read)
*           Maximum length of text to store in a group element.
*        ASTLN = INTEGER (Read and Write)
*           Next element to use in the group holding AST_ data.

*  Global Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER SZTEXT             ! Size of text buffer
      PARAMETER ( SZTEXT = ( GRP__SZNAM - 1 ) * ( KPG__MXACL + 1 ) )

*  Local Variables:
      CHARACTER * ( 1 ) FLAG     ! Flag character
      CHARACTER * ( SZTEXT + 1 ) TEXT ! Buffer for AST_ text
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
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ACL', KPG__MXACL )
            CALL ERR_REP( 'KPG1_SNKTA_XS',
     :           'Maximum number of continuation lines (^ACL) ' //
     :           'exceeded - output text is too long.',
     :           STATUS )
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
                  CALL GRP_PUT( ASTGRP, 1, FLAG // TEXT( I1 : I2 ),
     :                          ASTLN, STATUS )

*  Increment the line number to be used next, and note that the next
*  line (if any) will be a continuation line.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     ASTLN = ASTLN + 1
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
