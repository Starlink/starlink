      SUBROUTINE SST_TRHTM( ATASK, LATEX, PREFOR, STATUS )
*+
*  Name:
*     SST_TRHTM

*  Purpose:
*     Translate prologue information into html/LaTeX format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_TRHTM( ATASK, STATUS )

*  Description:
*     This routine translates prologue information held in the internal
*     source code buffer into part of an html document, that might be
*     included as part of a LaTeX document.

*  Arguments:
*     ATASK = LOGICAL (Given)
*        Whether an A-task (i.e. application) prologue is being
*        processed, as opposed to an ordinary subroutine or function
*        which might appear in a subroutine library. Slightly different
*        prologue analysis takes place in each case.
*     LATEX = LOGICAL (Given)
*        Whether the resulting document should include formatting
*        suitable for processing by the LaTeX2html package.
*     PREFOR = LOGICAL (Given)
*        Whether to preserve the formatting of the input file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-DEC-1994 (PDRAPER):
*        Original version. Converted from RFWS's SST_TRLAT.
*     14-APR-2005 (PDRAPER):
*        Converted to use pre-defined backslash character.
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
      INCLUDE 'SST_SCB'          ! SST Source Code Buffer

*  Arguments Given:
      LOGICAL ATASK
      LOGICAL LATEX
      LOGICAL PREFOR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXKEY              ! Max. no of search keys
      PARAMETER ( MXKEY = 18 )

      INTEGER SZKEY              ! Size of search keys
      PARAMETER ( SZKEY = 30 )

*  Local Variables:
      CHARACTER * ( SST__SZLIN ) BUF ! Local buffer
      CHARACTER * ( SZKEY ) NAME( MXKEY ) ! Search key for sections
      INTEGER FIRST              ! First line number in section body
      INTEGER HEADER             ! Section header line number
      INTEGER LAST               ! Last line number in section body
      INTEGER NC                 ! Number of characters in line

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Name:
*  ====
*  Locate the routine's name section.
      HEADER = 0
      NAME( 1 ) = 'Name'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no name was found, then report an error and give up.
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .NE. LAST ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SST_TRHTM_NAME',
     :                 'Missing or invalid prologue "Name:" section.',
     :                 STATUS )
         GO TO 99
      END IF

*  Begin the html routine description environment. If using LaTeX then
*  add a subsection and begin a rawhtml environment. Otherwise add a
*  level 1 heading with the routine name (latex2html will produce one
*  from the subsection command).
      IF ( LATEX ) THEN
         BUF = SST__BKSLH // 'subsection{'//
     :         SCB_LINE( FIRST )( SCB_FC( FIRST ) : SCB_LC( FIRST ) )//
     :         '}'
         CALL SST_PUT( 0, BUF, STATUS )
         CALL SST_PUT( 0, SST__BKSLH // 'begin{rawhtml}', STATUS )
      ELSE
         BUF = '<H1>'//
     :         SCB_LINE( FIRST )( SCB_FC( FIRST ) : SCB_LC( FIRST ) )//
     :         '</H1>'
         CALL SST_PUT( 0, BUF, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Purpose:
*  =======
*  Locate the routine's purpose section.
      HEADER = 0
      NAME( 1 ) = 'Purpose'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no purpose section was found, then report an error and give up.
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SST_TRHTM_PURP',
     :                 'No prologue "Purpose:" section found.', STATUS )
         GO TO 99
      END IF

*  Remove any trailing full stop from the purpose description and
*  output the section body in paragraph mode.
      IF ( SCB_LINE( LAST )( SCB_LC( LAST ) : SCB_LC( LAST ) )
     :     .EQ. '.' ) THEN
         SCB_LC( LAST ) = SCB_LC( LAST ) - 1
      ENDIF
      CALL SST_PUT( 0, '<H3>Purpose</H3>', STATUS )
      IF ( PREFOR ) THEN
         CALL SST_PUT( 3, '<PRE>', STATUS )
         CALL SST_PUTP( 3, FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '</PRE>', STATUS )
      ELSE
         CALL SST_HTMLP( 3, FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '<P>', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Description:
*  ===========
*  Locate the routine's description section.
      HEADER = 0
      NAME( 1 ) = 'Description'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no description section was found, then report an error and give
*  up.
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SST_TRHTM_DESC',
     :                 'No prologue "Description:" section found.',
     :                 STATUS )
         GO TO 99
      END IF

*  Output the body of the description section in paragraph mode.
      CALL SST_PUT( 0, '<H3>Description</H3>', STATUS )
      IF ( PREFOR ) THEN
         CALL SST_PUT( 3, '<PRE>', STATUS )
         CALL SST_PUTP( 3, FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '</PRE>', STATUS )
      ELSE
         CALL SST_HTMLP( 6, FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '<P>', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Usage:
*  =====
*  If processing an A-task prologue, locate the routine's usage section.
      IF ( ATASK ) THEN
         HEADER = 0
         NAME( 1 ) = 'Usage'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If a usage section was found, then output the body of the section in
*  paragraph mode.
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
            CALL SST_PUT( 0, '<H3>Usage</H3>', STATUS )
            CALL SST_PUT( 3, '<PRE>', STATUS )
            CALL SST_PUTP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '</PRE>', STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 99
         END IF

*  Invocation:
*  ==========
*  If not processing an A-task prologue, locate the routine's
*  invocation section.
      ELSE
         HEADER = 0
         NAME( 1 ) = 'Invocation'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no invocation section was found, then report an error and give up.
         IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SST_TRHTM_INVK',
     :                    'No prologue "Invocation:" section found.',
     :                    STATUS )
            GO TO 99
         END IF

*  Output the body of the invocation description in paragraph mode.
         CALL SST_PUT( 0, '<H3>Invocation</H3>', STATUS )
         CALL SST_PUT( 3, '<PRE>', STATUS )
         CALL SST_PUTP( 3, FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '</PRE>', STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  ADAM Parameters:/Arguments:
*  ==========================
*  If processing an A-task prologue, locate the routine's ADAM
*  parameters section. Otherwise, locate the arguments section instead.
      HEADER = 0
      IF ( ATASK ) THEN
         NAME( 1 ) = 'ADAM Parameters'
      ELSE
         NAME( 1 ) = 'Arguments'
      END IF
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no ADAM parameters/arguments section was found, then skip it.
*  Otherwise, output the body of the section in subsection mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         IF ( ATASK ) THEN
            CALL SST_PUT( 0, '<H3>ADAM parameters</H3>', STATUS )
            IF ( PREFOR ) THEN
               CALL SST_PUT( 3, '<PRE>', STATUS )
               CALL SST_PUTS( 3, 3, .TRUE., FIRST, LAST, STATUS )
               CALL SST_PUT( 3, '</PRE>', STATUS )
            ELSE
               CALL SST_HTMLS( 3, 3, .TRUE., FIRST, LAST, STATUS )
               CALL SST_PUT( 3, '<P>', STATUS )
            END IF
         ELSE
            CALL SST_PUT( 0, '<H3>Arguments</H3>', STATUS )
            IF ( PREFOR ) THEN
               CALL SST_PUT( 3, '<PRE>', STATUS )
               CALL SST_PUTS( 3, 3, .TRUE., FIRST, LAST, STATUS )
               CALL SST_PUT( 3, '</PRE>', STATUS )
            ELSE
               CALL SST_HTMLS( 3, 3, .TRUE., FIRST, LAST, STATUS )
               CALL SST_PUT( 3, '<P>', STATUS )
            END IF
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Returned Value:
*  ==============
*  If not processing an A-task prologue, then locate the routine's
*  returned value section (only applies to functions).
      IF ( .NOT. ATASK ) THEN
         HEADER = 0
         NAME( 1 ) = 'Returned Value'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no returned value section was found, then skip it. Otherwise,
*  output the body of the section in subsection mode.
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
            CALL SST_PUT( 0, '<H3>Returned value</H3>', STATUS )
            IF ( PREFOR ) THEN
               CALL SST_PUT( 3, '<PRE>', STATUS )
               CALL SST_PUTS( 3, 3, .TRUE., FIRST, LAST, STATUS )
               CALL SST_PUT( 3, '</PRE>', STATUS )
            ELSE
               CALL SST_HTMLS( 3, 3, .TRUE., FIRST, LAST, STATUS )
               CALL SST_PUT( 3, '<P>', STATUS )
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  Examples:
*  ========
*  Locate the routine's examples section.
      HEADER = 0
      NAME( 1 ) = 'Examples'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no examples section was found, then skip it. Otherwise, output the
*  body of the section in subsection mode (using the special examples
*  format).
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '<H3>Examples</H3>', STATUS )
         IF ( PREFOR ) THEN
            CALL SST_PUT( 3, '<PRE>', STATUS )
            CALL SST_PUTS( 3, 3, .TRUE., FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '</PRE>', STATUS )
         ELSE
            CALL SST_HTMLX( 3, 3, .TRUE., FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '<P>', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Notes:
*  =====
*  Locate the routine's notes section.
      HEADER = 0
      NAME( 1 ) = 'Notes'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no notes section was found, then skip it. Otherwise, output the
*  body of the section in paragraph mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '<H3>Notes</H3>', STATUS )
         IF ( PREFOR ) THEN
            CALL SST_PUT( 3, '<PRE>', STATUS )
            CALL SST_PUTP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '</PRE>', STATUS )
         ELSE
            CALL SST_HTMLP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '<P>', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Anything else.
*  =============

*  Inhibit detection of those sections which are processed explicitly.
      HEADER = 0
      NAME( 1 ) = 'Name'
      NAME( 2 ) = 'Purpose'
      NAME( 3 ) = 'Usage'
      NAME( 4 ) = 'Invocation'
      NAME( 5 ) = 'Description'
      NAME( 6 ) = 'ADAM Parameters'
      NAME( 7 ) = 'Arguments'
      NAME( 8 ) = 'Returned Value'
      NAME( 9 ) = 'Examples'
      NAME( 10 ) = 'Notes'
      NAME( 11 ) = 'Authors'
      NAME( 12 ) = 'History'
      NAME( 13 ) = 'Implementation Status'
      NAME( 14 ) = 'Bugs'

*  Inhibit those not required at all.
      NAME( 15 ) = 'Type of Module'
      NAME( 16 ) = 'Algorithm'
      NAME( 17 ) = 'Implementation Deficiencies'
      NAME( 18 ) = 'Language'

*  Loop to find all remaining sections.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      CALL SST_NSECT( .FALSE., 18, NAME, HEADER, FIRST, LAST, STATUS )
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN

*  Remove any trailing colon from the section heading.
         BUF = '<H3>'
         NC = SCB_LC( HEADER ) - SCB_FC( HEADER ) + 5
         BUF( 5 : NC ) = SCB_LINE( HEADER )( SCB_FC( HEADER ) :
     :                                       SCB_LC( HEADER ) )
         IF ( BUF( NC : NC ) .EQ. ':' ) THEN
            BUF( NC : NC ) = ' '
         END IF
         BUF( NC : ) ='</H3>'
         NC = NC + 5

*  Output the heading, followed by the body of the section in paragraph
*  mode.
         CALL SST_PUT( 0, BUF( : NC ), STATUS )
         IF ( PREFOR ) THEN
            CALL SST_PUT( 3, '<PRE>', STATUS )
            CALL SST_PUTP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '</PRE>', STATUS )
         ELSE
            CALL SST_HTMLP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '<P>', STATUS )
         END IF
         GO TO 1
      END IF

*  Implementation Status:
*  =====================
*  Locate the routine's implementation status section.
      HEADER = 0
      NAME( 1 ) = 'Implementation Status'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no implementation status section was found, then skip it.
*  Otherwise, output the section body in paragraph mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '<H3>Implementation Status</H3>', STATUS )
         IF ( PREFOR ) THEN
            CALL SST_PUT( 3, '<PRE>', STATUS )
            CALL SST_PUTP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '</PRE>', STATUS )
         ELSE
            CALL SST_HTMLP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '<P>', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Bugs:
*  ====
*  Locate the routine's bugs section.
      HEADER = 0
      NAME( 1 ) = 'Bugs'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no bugs section was found, then skip it. Otherwise, output the
*  section body in paragraph mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '<H3>Bugs</H3>', STATUS )
         IF ( PREFOR ) THEN
            CALL SST_PUT( 3, '<PRE>', STATUS )
            CALL SST_PUTP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '</PRE>', STATUS )
         ELSE
            CALL SST_HTMLP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '<P>', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  End output.
      IF ( LATEX ) THEN
         CALL SST_PUT( 0, SST__BKSLH // 'end{rawhtml}', STATUS )
      END IF
      CALL SST_PUT( 0, '<P>', STATUS )

99    CONTINUE

* @(#)sst_trhtm.f   1.6   95/03/06 10:56:51   96/07/05 10:27:33
      END
