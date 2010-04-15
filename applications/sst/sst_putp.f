      SUBROUTINE SST_PUTP( INDENT, FIRST, LAST, STATUS )
*+
*  Name:
*     SST_PUTP

*  Purpose:
*     Output the body of a section formatted in "paragraph" mode.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_PUTP( INDENT, FIRST, LAST, STATUS )

*  Description:
*     The routine sends all the lines in the body of a prologue section
*     formatted in paragraph mode to the output file, with indentation
*     adjusted to the specified amount. The body of the section is
*     identified by its first and last line numbers in the internal
*     source code buffer. Relative indentation within the section is
*     preserved, but the "base" level is set by the routine argument
*     INDENT, which specifies the number of leading blanks to be
*     inserted.

*  Arguments:
*     INDENT = INTEGER (Given)
*        Level of indentation required.
*     FIRST = INTEGER (Given)
*        First line number.
*     LAST = INTEGER (Given)
*        Last line number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-DEC-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ constants

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST_ Source Code Buffer

*  Arguments Given:
      INTEGER INDENT
      INTEGER FIRST
      INTEGER LAST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BASE               ! Base level of indentation
      INTEGER I                  ! Loop counter for output lines
      LOGICAL PREVBL             ! Previous output line was blank?

*  Internal References:
      LOGICAL BLANK              ! Whether line is blank
      INTEGER LINE               ! Dummy argument for BLANK
      BLANK( LINE ) = SCB_FC( LINE ) .GT. SCB_LC( LINE )

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the base level of indentation.
      BASE = SST__SZLIN

*  Find the minimum level of indentation within the body of the section.
      DO 1 I = FIRST, LAST
         IF ( .NOT. BLANK( I ) ) THEN
            BASE = MIN( BASE, SCB_FC( I ) )
         END IF
1     CONTINUE

*  Initialise flag indicating whether the previous line was blank.
      PREVBL = .TRUE.

*  Loop to output the lines in the section. Blank lines simply result in
*  a blank output line, unless the previous line was blank anyway.
      DO 2 I = FIRST, LAST
         IF ( BLANK( I ) ) THEN
            IF ( .NOT. PREVBL ) THEN
               CALL SST_PUT( 0, ' ', STATUS )
               PREVBL = .TRUE.
            END IF

*  Non-blank lines are output with the base level of indentation
*  adjusted to equal INDENT. If the line begins with a '-', then this
*  is the start of an item within an itemised list, so insert a blank
*  line to separate it (unless the previous line is already blank).
         ELSE
            IF ( ( SCB_LINE( I )( SCB_FC( I ) :
     :                            SCB_FC( I ) ) .EQ. '-' ) .AND.
     :           ( .NOT. PREVBL ) ) THEN
               CALL SST_PUT( 0, ' ', STATUS )
            END IF
            CALL SST_PUT( INDENT + SCB_FC( I ) - BASE,
     :                    SCB_LINE( I )( SCB_FC( I ) : SCB_LC( I ) ),
     :                    STATUS )
            PREVBL = .FALSE.
         END IF
2     CONTINUE

      END
* @(#)sst_putp.f   1.1   94/12/05 11:31:32   96/07/05 10:27:32
