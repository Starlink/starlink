      SUBROUTINE SST_TRLAT( ATASK, STATUS )
*+
*  Name:
*     SST_TRLAT

*  Purpose:
*     Translate prologue information into Latex format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_TRLAT( ATASK, STATUS )

*  Description:
*     This routine translates prologue information held in the internal
*     source code buffer into part of a Latex document, which is written
*     to the output file.

*  Arguments:
*     ATASK = LOGICAL (Given)
*        Whether an A-task (i.e. application) prologue is being
*        processed, as opposed to an ordinary subroutine or function
*        which might appear in a subroutine library. Slightly different
*        prologue analysis takes place in each case.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1994 Science & Engineering Research Council.
*     Copyright (C) 2005, 2006  Particle Physics & Engineering Research
*        Council.
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
*     DSB: David S. Berry
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-AUG-1990 (RFWS):
*        Original version.
*     13-AUG-1990 (RFWS):
*        Added support for required and positional parameters sections
*        in A-task prologues.
*     6-SEP-1990 (RFWS):
*        Changed to call SST_LATEX to output the examples section.
*     10-SEP-1990 (RFWS):
*        Reverted to an earlier version without support for the
*        required or positional parameters sections (until a convincing
*        way of handling them can be devised). Added support for the
*        "Implementation Status" section.
*     12-SEP-1990 (RFWS):
*        Added support for an A-task "Usage" section.
*     5-DEC-1994 (PDRAPER):
*        Added double \ for UNIX port.
*     14-APR-2005 (PDRAPER):
*        Made double \ sensitive to the compiler capabilities.
*     28-SEP-2005 (DSB):
*        Ignored "Synopsis" sections.
*     2006 July 12 (MJC):
*        Ignore "Licence" and "Copyright" sections.
*     21-JUL-2009 (TIMJ):
*        If an unrecognized section includes the string "Parameters" treat
*        it as a formatted subsection similar to "ADAM Parameters". Uses the
*        new sstdiylist LaTeX command.
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
      PARAMETER ( MXKEY = 22 )

      INTEGER SZKEY              ! Size of search keys
      PARAMETER ( SZKEY = 30 )

*  Local Variables:
      CHARACTER * ( SST__SZLIN ) TOPIC ! DIY section topic
      CHARACTER * ( SZKEY ) NAME( MXKEY ) ! Search key for sections
      INTEGER FIRST              ! First line number in section body
      INTEGER HEADER             ! Section header line number
      INTEGER LAST               ! Last line number in section body
      INTEGER NC                 ! Number of characters in line
      LOGICAL ISLIST             ! Is the DIY topic a list?

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
         CALL ERR_REP( 'SST_TRLAT_NAME',
     :                 'Missing or invalid prologue "Name:" section.',
     :                 STATUS )
         GO TO 99
      END IF

*  Begin the Latex routine description environment and output the name
*  of the routine.
      CALL SST_PUT( 0, SST__BKSLH // 'sstroutine{', STATUS )
      CALL SST_LAT( 3, SCB_LINE( FIRST )( SCB_FC( FIRST ) :
     :                                    SCB_LC( FIRST ) ),
     :              STATUS )
      CALL SST_PUT( 0, '}{', STATUS )
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
         CALL ERR_REP( 'SST_TRLAT_PURP',
     :                 'No prologue "Purpose:" section found.', STATUS )
         GO TO 99
      END IF

*  Remove any trailing full stop from the purpose description and
*  output the section body in paragraph mode.
      IF ( SCB_LINE( LAST )( SCB_LC( LAST ) : SCB_LC( LAST ) )
     :     .EQ. '.' ) THEN
         SCB_LC( LAST ) = SCB_LC( LAST ) - 1
      ENDIF
      CALL SST_LATP( 3, FIRST, LAST, STATUS )
      CALL SST_PUT( 0, '}{', STATUS )
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
         CALL ERR_REP( 'SST_TRLAT_DESC',
     :                 'No prologue "Description:" section found.',
     :                 STATUS )
         GO TO 99
      END IF

*  Output the body of the description section in paragraph mode.
      CALL SST_PUT( 3, SST__BKSLH // 'sstdescription{', STATUS )
      CALL SST_LATP( 6, FIRST, LAST, STATUS )
      CALL SST_PUT( 3, '}', STATUS )
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
            CALL SST_PUT( 3, SST__BKSLH // 'sstusage{', STATUS )
            CALL SST_LATP( 6, FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '}', STATUS )
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
            CALL ERR_REP( 'SST_TRLAT_INVK',
     :                    'No prologue "Invocation:" section found.',
     :                    STATUS )
            GO TO 99
         END IF

*  Output the body of the invocation description in paragraph mode.
         CALL SST_PUT( 3, SST__BKSLH // 'sstinvocation{', STATUS )
         CALL SST_LATP( 6, FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '}', STATUS )
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
            CALL SST_PUT( 3, SST__BKSLH // 'sstparameters{', STATUS )
            CALL SST_LATS( 6, 3, .TRUE., FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '}', STATUS )
         ELSE
            CALL SST_PUT( 3, SST__BKSLH // 'sstarguments{', STATUS )
            CALL SST_LATS( 6, 3, .TRUE., FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '}', STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Applicability:
*  ==============
*  If processing an A-task prologue, locate the routine's "Applicability"
*  section.
      HEADER = 0
      NAME( 1 ) = 'Applicability'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If no "Applicability" section was found, then skip it.  Otherwise,
*  output the body of the section in subsection mode.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_PUT( 3, SST__BKSLH // 'sstapplicability{', STATUS )
         CALL SST_LATS( 6, 3, .TRUE., FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '}', STATUS )
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
            CALL SST_PUT( 3, SST__BKSLH // 'sstreturnedvalue{', STATUS )
            CALL SST_LATS( 6, 3, .TRUE., FIRST, LAST, STATUS )
            CALL SST_PUT( 3, '}', STATUS )
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
         CALL SST_PUT( 3, SST__BKSLH // 'sstexamples{', STATUS )
         CALL SST_LATEX( 6, 3, .TRUE., FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '}', STATUS )
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
         CALL SST_PUT( 3, SST__BKSLH // 'sstnotes{', STATUS )
         CALL SST_LATP( 6, FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '}', STATUS )
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
      NAME( 7 ) = 'Applicability'
      NAME( 8 ) = 'Arguments'
      NAME( 9 ) = 'Returned Value'
      NAME( 10 ) = 'Examples'
      NAME( 11 ) = 'Notes'
      NAME( 12 ) = 'Authors'
      NAME( 13 ) = 'History'
      NAME( 14 ) = 'Implementation Status'
      NAME( 15 ) = 'Bugs'

*  Inhibit those not required at all.
      NAME( 16 ) = 'Type of Module'
      NAME( 17 ) = 'Algorithm'
      NAME( 18 ) = 'Implementation Deficiencies'
      NAME( 19 ) = 'Language'
      NAME( 20 ) = 'Synopsis'
      NAME( 21 ) = 'Licence'
      NAME( 22 ) = 'Copyright'

*  Loop to find all remaining sections.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      CALL SST_NSECT( .FALSE., 22, NAME, HEADER, FIRST, LAST, STATUS )
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN

*  Remove any trailing colon from the section heading.
         NC = SCB_LC( HEADER ) - SCB_FC( HEADER ) + 1
         TOPIC( : NC ) = SCB_LINE( HEADER )( SCB_FC( HEADER ) :
     :                                       SCB_LC( HEADER ) )
         IF ( TOPIC( NC : NC ) .EQ. ':' ) THEN
            TOPIC( NC : NC ) = ' '
         END IF

*  See if the TOPIC contains the word "Parameters". If it is a parameter
*  section we treat it similarly to ADAM Parameters and use a subsection.
         IF ( INDEX( TOPIC( : NC ), "Parameters" ) .NE. 0 ) THEN
            ISLIST = .TRUE.
         ELSE
            ISLIST = .FALSE.
         END IF

*  Output the heading, followed by the body of the section in paragraph
*  mode.
         IF ( ISLIST ) THEN
            CALL SST_PUT( 3, SST__BKSLH // 'sstdiylist{', STATUS )
         ELSE
            CALL SST_PUT( 3, SST__BKSLH // 'sstdiytopic{', STATUS )
         END IF
         CALL SST_LAT( 6, TOPIC( : NC ), STATUS )
         CALL SST_PUT( 3, '}{', STATUS )
         IF ( ISLIST ) THEN
            CALL SST_LATS( 6, 3, .TRUE., FIRST, LAST, STATUS )
         ELSE
            CALL SST_LATP( 6, FIRST, LAST, STATUS )
         END IF
         CALL SST_PUT( 3, '}', STATUS )
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
         CALL SST_PUT( 3, SST__BKSLH // 'sstimplementationstatus{',
     :                 STATUS )
         CALL SST_LATP( 6, FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '}', STATUS )
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
         CALL SST_PUT( 3, SST__BKSLH // 'sstbugs{', STATUS )
         CALL SST_LATP( 6, FIRST, LAST, STATUS )
         CALL SST_PUT( 3, '}', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  End Latex output.
      CALL SST_PUT( 0, '}', STATUS )

99    CONTINUE
      END
* @(#)sst_trlat.f   1.2   94/12/05 11:58:49   96/07/05 10:27:27
