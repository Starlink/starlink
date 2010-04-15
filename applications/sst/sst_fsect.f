      SUBROUTINE SST_FSECT( HEADER, FIRST, LAST, STATUS )
*+
*  Name:
*     SST_FSECT

*  Purpose:
*     Find the next prologue section in the source code buffer.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_FSECT( HEADER, FIRST, LAST, STATUS )

*  Description:
*     The routine searches prologue lines in the source code buffer,
*     starting at line HEADER+1 to find a new section header line which
*     is non-blank and is indented no less than the initial HEADER line
*     (or is simply non-blank if HEADER is initially zero). The
*     resulting line number is returned in HEADER. The routine then
*     locates the first and last non-blank lines in the body of the
*     section (which terminates at the next header as defined by the
*     algorithm above) and returns these line numbers in FIRST and
*     LAST.

*  Arguments:
*     HEADER = INTEGER (Given and Returned)
*        Line number of the header line.
*     FIRST = INTEGER (Returned)
*        First line of section body.
*     LAST = INTEGER (Returned)
*        Last line of section body.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If no header is found, HEADER is returned as zero.
*     -  If the section identified has no body, then FIRST is returned
*     greater than LAST.

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
*     20-DEC-1989 (RFWS):
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
      INCLUDE 'SST_SCB'          ! SST_ Source Code Buffer.

*  Arguments Given and Returned:
      INTEGER HEADER

*  Arguments Returned:
      INTEGER FIRST
      INTEGER LAST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for SCB lines
      INTEGER INDENT             ! Target indentation for header
      INTEGER START              ! Initial HEADER value

*  Internal References:
      LOGICAL BLANK              ! Whether SCB line is blank
      INTEGER LINE               ! Dummy argument for BLANK
      BLANK( LINE ) = SCB_FC( LINE ) .GT. SCB_LC( LINE )

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the indent for the starting line, setting a default beyond the
*  end of the line if starting off (at line zero) or if the starting
*  line is blank.
      START = HEADER
      IF ( ( START .LE. 0 ) .OR. ( START .GT. SCB_NLINE ) ) THEN
         INDENT = SST__SZLIN + 1
      ELSE IF ( .NOT. BLANK( START ) ) THEN
         INDENT = SCB_FC( START )
      ELSE
         INDENT = SST__MXLIN + 1
      END IF

*  Search for the next non-blank line with an indent no larger than the
*  starting line.
      DO 1 HEADER = MAX( START + 1, 1 ), SCB_NLINE
         IF ( .NOT. BLANK( HEADER ) ) THEN
            IF ( SCB_FC( HEADER ) .LE. INDENT ) THEN
               GO TO 2
            END IF
         END IF
1     CONTINUE

*  Note if no new header line was found.
      HEADER = 0
2     CONTINUE

*  If found, then search again to find the last non-blank line in the
*  body of the section.
      IF ( HEADER .NE. 0 ) THEN
         LAST = 0
         DO 3 I = HEADER + 1, SCB_NLINE

*  Update LAST whenever a non-blank line is found with an indent
*  greater than the header line. Finish searching when a new header is
*  found with an indent no greater than the previous header line.
            IF ( .NOT. BLANK( I ) ) THEN
               IF ( SCB_FC( I ) .GT. SCB_FC( HEADER ) ) THEN
                  LAST = I
               ELSE
                  GO TO 4
               END IF
            END IF
3        CONTINUE
4        CONTINUE

*  Finally, search for the first non-blank line in the body of the
*  section.
         FIRST = LAST + 1
         DO 5 I = HEADER + 1, LAST
            IF ( .NOT. BLANK( I ) ) THEN
               FIRST = I
               GO TO 6
            END IF
5        CONTINUE
6        CONTINUE
      END IF

      END
* @(#)sst_fsect.f   1.1   94/12/05 11:31:25   96/07/05 10:27:31
