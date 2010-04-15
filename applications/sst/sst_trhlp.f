      SUBROUTINE SST_TRHLP( ATASK, STATUS )
*+
*  Name:
*     SST_TRHLP

*  Purpose:
*     Translate prologue information into help library format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_TRHLP( ATASK, STATUS )

*  Description:
*     This routine translates prologue information held in the internal
*     source code buffer into a help library entry, which is written to
*     the output file.

*  Arguments:
*     ATASK = LOGICAL (Given)
*        Whether an A-task (i.e. application) prologue is being
*        processed, as opposed to an ordinary subroutine or function
*        which might appear in a subroutine library. Slightly different
*        prologue analysis takes place in each case.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     14-AUG-1990 (RFWS):
*        Original version, derived from the SST_TRLAT routine.
*     15-AUG-1990 (RFWS):
*        Changed to output individual ADAM parameters as level 3 help
*        keys.
*     15-AUG-1990 (RFWS):
*        Changed to use the required and positional parameter sections
*        and to store the information within the level 2 "Parameters"
*        topic.
*     3-SEP-1990 (RFWS):
*        Changed to allow history subsections to consist of a header
*        only.
*     6-SEP-1990 (RFWS):
*        Added removal of embedded parentheses from top-level A-task
*        help keys.
*     7-SEP-1990 (RFWS):
*        Improved error message and fixed use of incorrect error names.
*     7-SEP-1990 (RFWS):
*        Improved the format of prompted and positional parameter
*        output.
*     10-SEP-1990 (RFWS):
*        Reverted to a previous version which does not use the
*        positional and required parameters sections (until a
*        convincing way of handling them can be found). Added support
*        for the "Implementation Status" section.
*     13-SEP-1990 (RFWS):
*        Added support for A-task "Usage:" sections.
*     28-SEP-2005 (DSB):
*        Ignore "Synopsis" sections.
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
      INCLUDE 'SST_SCB'          ! SST Source Code Buffer

*  Arguments Given:
      LOGICAL ATASK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXKEY              ! Max. no of search keys
      PARAMETER ( MXKEY = 19 )

      INTEGER SZKEY              ! Size of search keys
      PARAMETER ( SZKEY = 30 )

*  Local Variables:
      CHARACTER * ( SST__SZLIN ) BUF ! Text buffer
      CHARACTER * ( SZKEY ) NAME( MXKEY ) ! Search key for sections
      INTEGER FIRST              ! First line number in section body
      INTEGER HEADER             ! Line number of section header
      INTEGER LAST               ! Last line number in section body
      INTEGER NC                 ! Number of characters in buffer

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
         CALL ERR_REP( 'SST_TRHLP_NAME',
     :                 'Missing or invalid prologue "Name:" section.',
     :                 STATUS )
         GO TO 99
      END IF

*  Extract the name. If processing an A-task prologue, then remove any
*  embedded parentheses (these may be used to indicate a command
*  abbreviation) and convert it to upper case. Output the result as a
*  top-level help keyword.
      NC = SCB_LC( FIRST ) - SCB_FC( FIRST ) + 1
      BUF( : NC ) = SCB_LINE( FIRST )( SCB_FC( FIRST ) :
     :                                    SCB_LC( FIRST ) )
      IF ( ATASK ) THEN
         CALL SST_RMCHR( '()', BUF( : NC ) )
         CALL CHR_UCASE( BUF( : NC ) )
      END IF
      CALL SST_PUT( 0, '1 ' // BUF( : NC ), STATUS )
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
         CALL ERR_REP( 'SST_TRHLP_PURP',
     :                 'No prologue "Purpose:" section found.', STATUS )
         GO TO 99
      END IF

*  Output the purpose description in paragraph mode, followed by a
*  blank line.
      CALL SST_PUTP( 0, FIRST, LAST, STATUS )
      CALL SST_PUT( 0, ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Usage:
*  =====
*  If processing an A-task prologue, then search for the usage section.
      IF ( ATASK ) THEN
         HEADER = 0
         NAME( 1 ) = 'Usage'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If a usage section was found, then output a heading, followed by the
*  body of the section in paragraph mode.
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
            CALL SST_PUT( 0, 'Usage:', STATUS )
            CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_PUTP( 3, FIRST, LAST, STATUS )
            CALL SST_PUT( 0, ' ', STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Invocation:
*  ==========
*  If not processing an A-task prologue, then locate the routine's
*  invocation section.
      ELSE
         HEADER = 0
         NAME( 1 ) = 'Invocation'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no invocation section was found, then report an error and give up.
         IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SST_TRHLP_INVK',
     :                    'No prologue "Invocation:" section found.',
     :                    STATUS )
            GO TO 99
         END IF

*  Output the invocation description in paragraph mode, followed by a
*  blank line.
         CALL SST_PUTP( 0, FIRST, LAST, STATUS )
         CALL SST_PUT( 0, ' ', STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

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
         CALL ERR_REP( 'SST_TRHLP_DESC',
     :                 'No prologue "Description:" section found.',
     :                 STATUS )
         GO TO 99
      END IF

*  Output the description header, followed by the indented description
*  body in paragraph mode.
      CALL SST_PUT( 0, 'Description:', STATUS )
      CALL SST_PUT( 0, ' ', STATUS )
      CALL SST_PUTP( 3, FIRST, LAST, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  ADAM Parameters:
*  ===============
*  If processing an A-task prologue, then locate the routine's ADAM
*  parameters section.
      IF ( ATASK ) THEN
         HEADER = 0
         NAME( 1 ) = 'ADAM Parameters'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no ADAM parameters section was found, then skip it. If found,
*  then output a level 2 help key.
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
            CALL SST_PUT( 0, '2 Parameters', STATUS )

*  Output the individual parameter descriptions as level 3 help keys.
            CALL SST_PUT( 0,
     :      'For information on individual parameters, select from ' //
     :      'the list below:', STATUS )
            CALL SST_HLPAP( FIRST, LAST, STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  Arguments:
*  =========
*  If not processing an A-task prologue, then locate the routine's
*  arguments section.
      IF ( .NOT. ATASK ) THEN
         HEADER = 0
         NAME( 1 ) = 'Arguments'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no arguments section was found, then skip it. If found, then
*  output the header as a level 2 help key, followed by the argument
*  descriptions in subsection mode.
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
            CALL SST_PUT( 0, '2 Arguments', STATUS )
            CALL SST_PUTS( 0, 3, .TRUE., FIRST, LAST, STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  Returned value:
*  ==============
*  If not processing an A-task prologue, then locate the routine's
*  returned value section (only applies to functions).
      IF ( .NOT. ATASK ) THEN
         HEADER = 0
         NAME( 1 ) = 'Returned Value'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no returned value section was found, then skip it. If found, then
*  output the header as a level 2 help key, followed by the returned
*  value description in subsection mode.
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
            CALL SST_PUT( 0, '2 Returned_Value', STATUS )
            CALL SST_PUTS( 0, 3, .TRUE., FIRST, LAST, STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  Examples:
*  ========
*  Locate the routine's examples section.
      HEADER = 0
      NAME( 1 ) = 'Examples'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no examples section was found, then skip it. If found, then
*  output the header as a level 2 help key, followed by the examples
*  themselves in subsection mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '2 Examples', STATUS )
         CALL SST_PUTS( 0, 3, .TRUE., FIRST, LAST, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Notes:
*  =====
*  Locate the routine's notes section.
      HEADER = 0
      NAME( 1 ) = 'Notes'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no notes section was found, then skip it. If found, then output
*  the header as a level 2 help key, followed by the notes in paragraph
*  mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '2 Notes', STATUS )
         CALL SST_PUTP( 0, FIRST, LAST, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Anything else.
*  =============

*  Inhibit those sections which are explicitly processed.
      HEADER = 0
      NAME( 1 ) = 'Name'
      NAME( 2 ) = 'Purpose'
      NAME( 3 ) = 'Invocation'
      NAME( 4 ) = 'Usage'
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
      NAME( 19 ) = 'Synopsis'

*  Loop to find all remaining sections.
1     CONTINUE                ! Start of 'DO WHILE' loop
      CALL SST_NSECT( .FALSE., 19, NAME, HEADER, FIRST, LAST, STATUS )
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN

*  Remove any trailing colon from the section heading.
         NC = SCB_LC( HEADER ) - SCB_FC( HEADER ) + 1
         BUF( : NC ) = SCB_LINE( HEADER )( SCB_FC( HEADER ) :
     :                                     SCB_LC( HEADER ) )
         IF ( BUF( NC : NC ) .EQ. ':' ) THEN
            BUF( NC : NC ) = ' '
         END IF
         CALL SST_HLPKY( BUF( : NC ), STATUS )

*  Output the section as a level 2 help topic.
         CALL SST_PUT( 0, '2 ' // BUF( : NC ), STATUS )
         CALL SST_PUTP( 3, FIRST, LAST, STATUS )
         GO TO 1
      END IF

*  Authors:
*  =======
*  Locate the routine's authors section.
      HEADER = 0
      NAME( 1 ) = 'Authors'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no authors section was found, then skip it. If found, then output
*  the header as a level 2 help key, followed by the authors
*  subsections, formatted in subsection mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '2 Authors', STATUS )
         CALL SST_PUTS( 0, 3, .TRUE., FIRST, LAST, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  History:
*  =======
*  Locate the routine's history section.
      HEADER = 0
      NAME( 1 ) = 'History'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no history section was found, then skip it. If found, then output
*  the header as a level 2 help key, followed by the history
*  subsections, formatted in subsection mode. Indent the subsection
*  headers by 1, as they may contain numerals in the first column which
*  upsets the help system.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '2 History', STATUS )
         CALL SST_PUTS( 1, 3, .TRUE., FIRST, LAST, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Implementation Status:
*  =====================
*  Locate the routine's implementation status section.
      HEADER = 0
      NAME( 1 ) = 'Implementation Status'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no implementation status section was found, then skip it. If
*  found, then output the header as a level 2 help key, followed by the
*  body of the section, formatted in paragraph mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '2 Implementation_Status', STATUS )
         CALL SST_PUTP( 3, FIRST, LAST, STATUS )
      END IF

*  Bugs:
*  ====
*  Locate the routine's bugs section.
      HEADER = 0
      NAME( 1 ) = 'Bugs'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no bugs section was found, then skip it. If found, then output
*  the header as a level 2 help key, followed by the body of the
*  section, formatted in paragraph mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 0, '2 Bugs', STATUS )
         CALL SST_PUTP( 3, FIRST, LAST, STATUS )
      END IF

99    CONTINUE
      END
* @(#)sst_trhlp.f   1.1   94/12/05 11:31:35   96/07/05 10:27:26
