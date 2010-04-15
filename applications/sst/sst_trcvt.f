      SUBROUTINE SST_TRCVT( ATASK, LANG, RNAME, RTYPE, STATUS )
*+
*  Name:
*     SST_TRCVT

*  Purpose:
*     Translate "old-style" ADAM/SSE prologues into STARLSE format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_TRCVT( ATASK, LANG, RNAME, RTYPE, STATUS )

*  Description:
*     This routine translates "old-style" ADAM/SSE prologue information
*     held in the internal source code buffer into STARLSE format,
*     writing it to the output file.

*  Arguments:
*     ATASK = LOGICAL (Given)
*        Whether an A-task (i.e. application) prologue is being
*        processed, as opposed to an ordinary subroutine or function
*        which might appear in a subroutine library. Slightly different
*        prologue analysis takes place in each case.
*     LANG = CHARACTER * ( * ) (Given)
*        The language (Fortran dialect) in which the routine is
*        written.  This will be added to the output prologue under the
*        "Language" section heading. If a blank value is given, then a
*        placeholder (to be filled in) will be generated instead.
*     RNAME = CHARACTER * ( * ) (Given)
*        The name of the program unit being processed.
*     RTYPE = CHARACTER * ( 1 ) (Given)
*        The program unit type: 'B', 'F', 'P', 'S' or '?' (in upper
*        case).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     16-AUG-1990 (RFWS):
*        Original version.
*     31-AUG-1990 (RFWS):
*        Added translation of ';' characters in invocation lines to
*        ','.
*     7-SEP-1990 (RFWS):
*        Changed to make invocation and returned value sections
*        dependent on the program unit type.
*     10-SEP-1990 (RFWS):
*        Added the LANG argument.
*     17-SEP-1990 (RFWS):
*        Changed to output the arguments section for an A-task
*        immediately after the invocation. Arguments for a non-A-task
*        remain after the description section.
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
      CHARACTER * ( * ) LANG
      CHARACTER * ( * ) RNAME
      CHARACTER * ( 1 ) RTYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER MXKEY              ! Max. no of search keys
      PARAMETER ( MXKEY = 12 )

      INTEGER SZKEY              ! Size of search keys
      PARAMETER ( SZKEY = 30 )

*  Local Variables:
      CHARACTER * ( SST__SZLIN ) BUF ! Output buffer
      CHARACTER * ( SST__SZLIN ) TOPIC ! DIY section topic
      CHARACTER * ( SZKEY ) NAME( MXKEY ) ! Search key for sections
      INTEGER FIRST              ! First line number in section body
      INTEGER HEADER             ! Section header line number
      INTEGER I                  ! Loop counter for invocation lines
      INTEGER LAST               ! Last line number in section body
      INTEGER LBUF               ! No. characters in output buffer
      INTEGER NC                 ! Number of characters in line

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Language:
*  ========
*  Write out the routine's language section, supplying a placeholder to
*  be filled in if necessary.
      CALL SST_FOR( 3, 'Language:', STATUS )
      IF ( LANG .NE. ' ' ) THEN
         CALL SST_FOR( 6, LANG, STATUS )
      ELSE
         CALL SST_FOR( 6, '{routine_language}', STATUS )
      END IF
      CALL SST_FOR( 0, ' ', STATUS )

*  Type of Module:
*  ==============
*  If an A-task is being processed, then write out a type of module
*  section.
      IF ( ATASK ) THEN
         CALL SST_FOR( 3, 'Type of Module:', STATUS )
         CALL SST_FOR( 6, 'ADAM A-task', STATUS )
         CALL SST_FOR( 0, ' ', STATUS )
      END IF

*  Invocation:
*  ==========
*  If this is not a block data routine or a program, then locate the
*  invocation section.
      IF ( ATASK .OR.
     :     ( ( RTYPE .NE. 'B' ) .AND. ( RTYPE .NE. 'P' ) ) ) THEN
         HEADER = 0
         NAME( 1 ) = 'Invocation'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out the invocation section, if found.
         CALL SST_FOR( 3, 'Invocation:', STATUS )
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN

*  Translate any ';' characters in the invocation lines to ',' before
*  sending them to the output file.
            DO 1 I = FIRST, LAST
               IF ( SCB_FC( I ) .LE. SCB_LC( I ) ) THEN
                  CALL SST_TRCHR( ';', ',', SCB_LINE( I )( SCB_FC( I ) :
     :                                                   SCB_LC( I ) ) )
               END IF
1           CONTINUE
            CALL SST_FORP( 6, FIRST, LAST, STATUS )

*  Supply an appropriate template if the actual invocation description
*  could not be found.
         ELSE
            NC = MAX( 1, CHR_LEN( RNAME ) )

*  Function invocation...
            IF ( RTYPE .EQ. 'F' ) THEN
               LBUF = 0
               CALL CHR_PUTC( 'RESULT = ', BUF, LBUF )
               CALL CHR_PUTC( RNAME( : NC ), BUF, LBUF )
               CALL CHR_PUTC( '( [p_]... )', BUF, LBUF )
               CALL SST_FOR( 6, BUF( : LBUF ), STATUS )

*  Subroutine invocation...
            ELSE
               LBUF = 0
               CALL CHR_PUTC( 'CALL ', BUF, LBUF )
               CALL CHR_PUTC( RNAME( : NC ), BUF, LBUF )
               CALL CHR_PUTC( '( [p]... )', BUF, LBUF )
               CALL SST_FOR( 6, BUF( : LBUF ), STATUS )
            END IF
         END IF
         CALL SST_FOR( 0, ' ', STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  Arguments: (1)
*  =========
*  If processing an A-task prologue, then write out an arguments
*  section describing the status variable.
      IF ( ATASK ) THEN
         CALL SST_FOR( 3, 'Arguments:', STATUS )
         CALL SST_FOR( 6, 'STATUS = INTEGER (Given and Returned)',
     :                 STATUS )
         CALL SST_FOR( 9, 'The global status.', STATUS )
         CALL SST_FOR( 0, ' ', STATUS )
      END IF

*  Description:
*  ===========
*  Locate the routine's description section.
      HEADER = 0
      NAME( 1 ) = 'Description'
      NAME( 2 ) = 'Purpose'
      CALL SST_NSECT( .TRUE., 2, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out the description section, supplying a placeholder if the
*  actual description could not be found.
      CALL SST_FOR( 3, 'Description:', STATUS )
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
         CALL SST_FOR( 6, '{routine_description}', STATUS )
      ELSE
         CALL SST_FORP( 6, FIRST, LAST, STATUS )
      END IF
      CALL SST_FOR( 0, ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Parameters:
*  ==========
*  If processing an A-task prologue, then locate the routine's
*  parameters section.
      IF ( ATASK ) THEN
         HEADER = 0
         NAME( 1 ) = 'Parameters'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out an "ADAM parameters" section if the necessary information
*  was found.
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
            CALL SST_FOR( 3, 'ADAM Parameters:', STATUS )
            CALL SST_FORS( 6, 3, .TRUE., FIRST, LAST, STATUS )
            CALL SST_FOR( 0, ' ', STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  Arguments: (2)
*  =========
*  If this is not an A-task prologue and is not a block data routine or
*  program, then search for the "parameters" or "arguments" section in
*  the prologue.
      IF ( ( .NOT. ATASK ) .AND.
     :     ( RTYPE .NE. 'B' ) .AND. ( RTYPE .NE. 'P' ) ) THEN
         HEADER = 0
         NAME( 1 ) = 'Parameters'
         NAME( 2 ) = 'Arguments'
         CALL SST_NSECT( .TRUE., 2, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out an arguments section if the necessary information was
*  found.
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
            CALL SST_FOR( 3, 'Arguments:', STATUS )
            CALL SST_FORS( 6, 3, .TRUE., FIRST, LAST, STATUS )
            CALL SST_FOR( 0, ' ', STATUS )
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  Returned Value:
*  ==============
*  If the program unit is a function, then locate the result section.
      IF ( RTYPE .EQ. 'F' ) THEN
         HEADER = 0
         NAME( 1 ) = 'Result'
         CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out a returned value section if the necessary information was
*  found.
         CALL SST_FOR( 3, 'Returned Value:', STATUS )
         IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
            CALL SST_FORS( 6, 3, .TRUE., FIRST, LAST, STATUS )

*  Otherwise supply a template.
         ELSE
            CALL SST_FOR( 6, '{routine_name} = {data_type}', STATUS )
            CALL SST_FOR( 9, '{returned_value_description}', STATUS )
         END IF
         CALL SST_FOR( 0, ' ', STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
      END IF

*  Algorithm:
*  =========
*  Locate the routine's "method" section.
      HEADER = 0
      NAME( 1 ) = 'Method'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out an algorithm section if the necessary information was
*  found.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_FOR( 3, 'Algorithm:', STATUS )
         CALL SST_FORP( 6, FIRST, LAST, STATUS )
         CALL SST_FOR( 0, ' ', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Deficiencies:
*  ============
*  Locate the routine's "deficiencies" section.
      HEADER = 0
      NAME( 1 ) = 'Deficiencies'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out an implementation deficiencies section if the necessary
*  information was found.
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN
         CALL SST_FOR( 3, 'Implementation Deficiencies:', STATUS )
         CALL SST_FORP( 6, FIRST, LAST, STATUS )
         CALL SST_FOR( 0, ' ', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  All the rest.
*  ============
*  Inhibit those sections which are explicitly processed.
      HEADER = 0
      NAME( 1 ) = 'Description'
      NAME( 2 ) = 'Purpose'
      NAME( 3 ) = 'Invocation'
      NAME( 4 ) = 'Parameters'
      NAME( 5 ) = 'Arguments'
      NAME( 6 ) = 'Result'
      NAME( 7 ) = 'Method'
      NAME( 8 ) = 'Deficiencies'
      NAME( 9 ) = 'Bugs'
      NAME( 10 ) = 'Authors'
      NAME( 11 ) = 'Author'
      NAME( 12 ) = 'History'

*  Loop to find all remaining sections.
2     CONTINUE                ! Start of 'DO WHILE' loop
      CALL SST_NSECT( .FALSE., 12, NAME, HEADER, FIRST, LAST, STATUS )
      IF ( ( HEADER .NE. 0 ) .AND. ( FIRST .LE. LAST ) ) THEN

*  Extract the section heading and remove any trailing colon from it.
         NC = SCB_LC( HEADER ) - SCB_FC( HEADER ) + 1
         TOPIC( : NC ) = SCB_LINE( HEADER )( SCB_FC( HEADER ) :
     :                                       SCB_LC( HEADER ) )
         IF ( TOPIC( NC : NC ) .EQ. ':' ) THEN
            TOPIC( NC : NC ) = ' '
            NC = MAX( 1, CHR_LEN( TOPIC( : NC ) ) )
         END IF

*  Capitalise the words of the section heading.
         CALL SST_CAPWD( TOPIC( : NC ), STATUS )

*  Output the section heading with a colon immediately after it,
*  followed by the body of the section in paragraph mode.
         CALL SST_FOR( 3, TOPIC( : NC ) // ':', STATUS )
         CALL SST_FORP( 6, FIRST, LAST, STATUS )
         CALL SST_FOR( 0, ' ', STATUS )
         GO TO 2
      END IF

*  Authors:
*  =======
*  Locate the routine's authors section.
      HEADER = 0
      NAME( 1 ) = 'Authors'
      NAME( 2 ) = 'Author'
      CALL SST_NSECT( .TRUE., 2, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out an authors section, supplying a suitable placeholder for
*  future entries.
      CALL SST_FOR( 3, 'Authors:', STATUS )
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
         CALL SST_FOR( 6, '{original_author_entry}', STATUS )
      ELSE
         CALL SST_FORS( 6, 3, .TRUE., FIRST, LAST, STATUS )
         CALL SST_FOR( 6, '{enter_new_authors_here}', STATUS )
      END IF
      CALL SST_FOR( 0, ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  History:
*  =======
*  Locate the routine's history section.
      HEADER = 0
      NAME( 1 ) = 'History'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out a history section, supplying a suitable placeholder for
*  future entries.
      CALL SST_FOR( 3, 'History:', STATUS )
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
         CALL SST_FOR( 6, '{enter_changes_here}', STATUS )
      ELSE
         CALL SST_FORS( 6, 3, .TRUE., FIRST, LAST, STATUS )
         CALL SST_FOR( 6, '{enter_further_changes_here}', STATUS )
      END IF
      CALL SST_FOR( 0, ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Bugs:
*  ====
*  Locate the routine's bugs section.
      HEADER = 0
      NAME( 1 ) = 'Bugs'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  Write out a bugs section, supplying a suitable placeholder for
*  future entries.
      CALL SST_FOR( 3, 'Bugs:', STATUS )
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
         CALL SST_FOR( 6, '{note_any_bugs_here}', STATUS )
      ELSE
         CALL SST_FORP( 6, FIRST, LAST, STATUS )
         CALL SST_FOR( 6, '{note_new_bugs_here}', STATUS )
      END IF
      CALL SST_FOR( 0, ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Write an end of prologue delimiter.
      CALL SST_FOR( 1, '-', STATUS )
      CALL SST_FOR( 0, ' ', STATUS )

99    CONTINUE
      END
* @(#)sst_trcvt.f   1.1   94/12/05 11:31:35   96/07/05 10:27:25
