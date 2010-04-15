      SUBROUTINE SST_LATEX( INDENT, INC, NULL, FIRST, LAST, STATUS )
*+
*  Name:
*     SST_LATEX

*  Purpose:
*     Output an examples section in Latex format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_LATEX( INDENT, INC, NULL, FIRST, LAST, STATUS )

*  Description:
*     The routine sends all the lines in the body of a prologue
*     examples section to the output file as Latex source.  Indentation
*     is adjusted to a specified level. The body of each example
*     description is output in paragraph mode.

*  Arguments:
*     INDENT = INTEGER (Given)
*        The level of indentation at which the example lines are to
*        appear.
*     INC = INTEGER (Given)
*        The additional indentation increment to be applied to the body
*        of each example description.
*     NULL = LOGICAL (Given)
*        Whether examples which have no description are to be output.
*     FIRST = INTEGER (Given)
*        First line number in the SCB to be processed.
*     LAST = INTEGER (Given)
*        Last line number in the SCB to be processed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1994 Science & Engineering Research Council.
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
*     6-SEP-1990 (RFWS):
*        Original, derived from the SST_LATS routine.
*     5-DEC-1994 (PDRAPER):
*        Added double \ for UNIX port.
*     14-APR-2005 (PDRAPER):
*        Converted to use pre-defined backslash character.
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
      INTEGER INC
      LOGICAL NULL
      INTEGER FIRST
      INTEGER LAST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! First line of description
      INTEGER FC                 ! Saved first character position
      INTEGER HEADER             ! Number of example header line
      INTEGER L                  ! Last line of description

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start searching for examples at the line before the first line to be
*  processed and set the first character position of this line (if it
*  exists) to beyond the end of the line, so that the FIRST line will
*  always be found initially. Save the first character position for
*  restoration later.
      HEADER = FIRST - 1
      IF ( FIRST .GT. 1 ) THEN
         FC = SCB_FC( FIRST - 1 )
         SCB_FC( FIRST - 1 ) = SST__SZLIN + 1
      ENDIF

*  Loop to process each example in the main section body.
1     CONTINUE                   ! Start of 'DO WHILE' loop

*  Find the next example.
      CALL SST_FSECT( HEADER, F, L, STATUS )

*  If found, then check if it should be output, according to the setting
*  of NULL.
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( HEADER .NE. 0 ) .AND.
     :     ( HEADER .LE. LAST ) ) THEN
         L = MIN( L, LAST )
         IF ( NULL .OR. ( F .LE. L ) ) THEN

*  Output the example header.
            CALL SST_PUT( INDENT, SST__BKSLH // 'sstexamplesubsection{',
     :                    STATUS )
            CALL SST_LAT( INDENT + INC,
     :                    SCB_LINE( HEADER )( SCB_FC( HEADER ) :
     :                                        SCB_LC( HEADER ) ),
     :                    STATUS )
            CALL SST_PUT( INDENT, '}{', STATUS )

*  Output the example description in paragraph mode.
            CALL SST_LATP( INDENT + INC, F, L, STATUS )
            CALL SST_PUT( INDENT, '}', STATUS )
         END IF
         GO TO 1
      END IF

*  Restore the first character position which was saved previously.
      IF ( FIRST .GT. 1 ) SCB_FC( FIRST - 1 ) = FC

      END
* @(#)sst_latex.f   1.2   94/12/05 11:58:44   96/07/05 10:27:25
