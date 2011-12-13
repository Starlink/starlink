      SUBROUTINE CCD1_WRTPA( PARA, LINELN, INDENT, LOG, STATUS )
*+
*  Name:
*     CCD1_WRTPA

*  Purpose:
*     Writes out a long line of information as a paragraph

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL CCD1_WRTPA( PARA, LINELN, INDENT, LOG, STATUS )

*  Description:
*     This routine accepts a long-line of characters and writes then
*     out using a maximum line-length. The output can be directed only
*     through the message system or it can be directed through the
*     CCDPACK message routines (possibly going to the logfile.

*  Arguments:
*     PARA = CHARACTER * ( * )  (Given)
*        The line of characters to be output as a paragraph.
*     LINELN = INTEGER (Given)
*        The maximum length of an output line.
*     INDENT = INTEGER (Given)
*        The left-hand indent applied to output lines.
*     LOG = LOGICAL (Given)
*        Whether to go through the CCDPACK message routines or not.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     21-MAY-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! SAE contants
      INCLUDE 'MSG_PAR'         ! MSG system constants

*  Arguments Given:
      CHARACTER * ( * ) PARA
      INTEGER LINELN
      INTEGER INDENT
      LOGICAL LOG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG ) LINE
      INTEGER IPOSN
      INTEGER LLEN
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      LLEN = MIN( MSG__SZMSG, LINELN )

*  Loop until all the characters in the line have been output.
      IPOSN = 1
 1    CONTINUE
      IF ( IPOSN .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         LINE = ' '
         CALL CHR_LINBR( PARA, IPOSN, LINE( INDENT : LLEN )  )
         IF ( LOG ) THEN
            CALL CCD1_MSG( ' ', LINE( :LLEN ), STATUS )
         ELSE
            CALL MSG_OUT( ' ', LINE( :LLEN ), STATUS )
         END IF
         GO TO 1
      END IF

      END
