      SUBROUTINE SST_LATP( INDENT, FIRST, LAST, STATUS )
*+
*  Name:
*     SST_LATP

*  Purpose:
*     Output the body of a section formatted in "paragraph" mode as
*     Latex.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_LATP( INDENT, FIRST, LAST, STATUS )

*  Description:
*     The routine sends all the lines in the body of a prologue section
*     formatted in paragraph mode to the output file as Latex source,
*     with indentation adjusted to the specified amount. The body of
*     the section is identified by its first and last line numbers in
*     the internal source code buffer. Relative indentation within the
*     section is preserved, but the "base" level is set by the routine
*     argument INDENT, which specifies the number of leading blanks to
*     be inserted. Output indentation is further adjusted to reflect the
*     logical structure of the Latex produced (which may include
*     itemised lists, for instance).

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
*     Copyright (C) 1989, 1994 Science & Engineering Research Council.
*     Copyright (C) 2005 Particls Physics & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     21-DEC-1989 (RFWS):
*        Original version.
*     5-DEC-1994 (PDRAPER):
*        Added double \ for UNIX port.
*     14-APR-2005 (PDRAPER):
*        Converted to use pre-defined backslash character.
*     1-MAY-2017 (DSB):
*        Preserve line breaks in the input when between "---" lines.
*        Indentation of the subsequent lines relative to the "---" marker
*        is retain by inserting appropriate horizontal space commands.
*     {enter_further_changes_here}

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
      CHARACTER * ( 3 * SST__SZLIN ) BUF ! Output buffer
      INTEGER BASE               ! Base level of indentation
      INTEGER BINDLB             ! Base level of indent for line-breaking
      INTEGER F                  ! First character of line to be output
      INTEGER I                  ! Loop counter for output lines
      INTEGER IND                ! Current output indentation
      INTEGER L                  ! Last character of line to be output
      INTEGER NC                 ! No. of characters in buffer
      INTEGER NSP                ! No. of leading spaces required
      LOGICAL ITEMS              ! Whether within an item list
      LOGICAL PREVBL             ! Previous output line was blank?
      LOGICAL PRLBRK             ! Preserve line breaks?
      LOGICAL USE                ! Include line in output ?

*  Internal References:
      LOGICAL BLANK              ! Whether line is blank
      INTEGER LINE               ! Dummy argument for BLANK
      BLANK( LINE ) = SCB_FC( LINE ) .GT. SCB_LC( LINE )

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the base level of input indentation and the current level
*  of output indentation.
      BASE = SST__SZLIN
      IND = INDENT

*  Find the minimum level of input indentation within the body of the
*  section.
      DO 1 I = FIRST, LAST
         IF ( .NOT. BLANK( I ) ) THEN
            BASE = MIN( BASE, SCB_FC( I ) )
         END IF
1     CONTINUE

*  Initialise flags indicating whether the previous line was blank,
*  whether we are within an itemised list, and whether we are preserving
*  line breaks.
      PREVBL = .TRUE.
      ITEMS = .FALSE.
      PRLBRK = .FALSE.

*  Loop to output the lines in the section.
      DO 2 I = FIRST, LAST

*  If we are preserving line-breaks, a blank input line simply
*  causes a newline command. Otherwise, it causes a blank output
*  line (unless the previous line was also blank).
         IF ( BLANK( I ) ) THEN
            IF ( PRLBRK ) THEN
               CALL SST_PUT( IND, SST__BKSLH // 'newline', STATUS )
            ELSE IF ( .NOT. PREVBL ) THEN
               CALL SST_PUT( 0, ' ', STATUS )
               PREVBL = .TRUE.
            END IF

* Otherwise, get the indices of the first and last non-blank characters.
         ELSE
            F = SCB_FC( I )
            L = SCB_LC( I )

*  Assume the line should be included in the output.
            USE = .TRUE.

*  If the line consists entirely of the string "---" then toggle the flag
*  that indicates if we are preserving line breaks.
            IF( SCB_LINE( I )( F : L ) .EQ. '---' ) THEN
               PRLBRK = .NOT. PRLBRK
               BINDLB = F
               CALL SST_PUT( IND, SST__BKSLH // 'newline', STATUS )
               IF( PRLBRK )
     :             CALL SST_PUT( IND, SST__BKSLH // 'newline', STATUS )
               USE = .FALSE.

*  If the line begins with a '-', then this is the start of an item
*  within an itemised list. Start this list if it has not already been
*  started and increase the output indentation level.
            ELSE IF ( ( SCB_LINE( I )( F : F ) .EQ. '-' ) ) THEN
               IF ( .NOT. ITEMS ) THEN
                  ITEMS = .TRUE.
                  CALL SST_PUT( IND, SST__BKSLH // 'sstitemlist{',
     :                          STATUS )
                  PREVBL = .FALSE.
                  IND = IND + 3
               END IF

*  Start the item, with a preceding blank line (if not already there).
               IF ( .NOT. PREVBL ) THEN
                  CALL SST_PUT( 0, ' ', STATUS )
               END IF
               CALL SST_PUT( IND, SST__BKSLH // 'sstitem', STATUS )

*  Note where to start the line so as to skip the '-'.
               F = F + 1

*  If the next line is not blank and does not begin with '-', but the
*  previous line was blank, then this marks the end of any itemised
*  list.  Terminate the current itemised list if it exists.
            ELSE IF ( PREVBL .AND. ITEMS ) THEN
               ITEMS = .FALSE.
               IND = IND - 3
               CALL SST_PUT( IND, '}', STATUS )
            END IF


*  Output usable lines.
            IF ( USE .AND. F .LE. L ) THEN

*  If we are preserving linebreaks also preserve leading spaces by
*  converting the leading spaces into a latex horizontal space.
               NSP = F - BINDLB
               IF( PRLBRK .AND. NSP .GT. 0 ) THEN
                  NC = 0
                  CALL CHR_APPND( SST__BKSLH // 'hspace*{', BUF, NC )
                  CALL CHR_PUTR( NSP/2.0, BUF, NC )
                  CALL CHR_APPND( ' em}', BUF, NC )
                  CALL SST_PUT( IND, BUF( : NC ), STATUS )
               END IF

*  Output the text with the base level of indentation adjusted
*  to equal IND.
               CALL SST_LAT( IND + F - BASE, SCB_LINE( I )( F : L ),
     :                       STATUS )

*  If we are preserving line breaks, output a latex newline command.
               IF( PRLBRK )
     :            CALL SST_PUT( IND, SST__BKSLH // 'newline', STATUS )

            END IF
            PREVBL = .FALSE.
         END IF
2     CONTINUE

*  Finally, terminate any current itemised list.
      IF ( ITEMS ) THEN
         CALL SST_PUT( IND - 3, '}', STATUS )
      END IF

      END
* @(#)sst_latp.f   1.2   94/12/05 11:58:45   96/07/05 10:27:26
