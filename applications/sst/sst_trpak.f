      SUBROUTINE SST_TRPAK( PACK, HELP, MXARG, ARG, NARG, STATUS )
*+
*  Name:
*     SST_TRPAK

*  Purpose:
*     Translate prologue information into an LSE package definition.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_TRPAK( PACK, HELP, MXARG, ARG, NARG, STATUS )

*  Description:
*     This routine translates prologue information held in the internal
*     source code buffer into part of an LSE package definition, which
*     is written to the output file. It also updates the list of
*     routine arguments supplied.

*  Arguments:
*     PACK = CHARACTER * ( * ) (Given)
*        The name of the package being defined.
*     HELP = LOGICAL (Given)
*        Whether help library references are required.
*     MXARG = INTEGER (Given)
*        Maximum number of routine arguments (i.e. the declared size of
*        the ARG array).
*     ARG( MXARG ) = CHARACTER * ( * ) (Given and Returned)
*        List of arguments for the package. Any arguments found by this
*        routine will be appended to the list, starting at element
*        NARG+1. Values are returned in upper case.
*     NARG = INTEGER (Given and Returned)
*        Number of arguments in the ARG array.  This value will be
*        updated to reflect the new arguments found by this routine.
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
*     7-SEP-1990 (RFWS):
*        Improved error message.
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
      CHARACTER * ( * ) PACK
      LOGICAL HELP
      INTEGER MXARG

*  Arguments Given and Returned:
      CHARACTER * ( * ) ARG( MXARG )
      INTEGER NARG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER SZKEY              ! Length of a search key
      PARAMETER ( SZKEY = 30 )

*  Local Variables:
      CHARACTER * ( 2 * SST__SZLIN ) BUF ! Text buffer
      CHARACTER * ( SZKEY ) NAME( 1 ) ! Prologue section search key
      INTEGER F                  ! Opening parenthesis position
      INTEGER FA                 ! First argument character position
      INTEGER FIRST              ! First line number in section body
      INTEGER HEADER             ! Section header line number
      INTEGER I                  ! Loop counter for arguments
      INTEGER ILINE              ! Loop counter for SCB lines
      INTEGER L                  ! Closing parenthesis position
      INTEGER LA                 ! Last argument character position
      INTEGER LARG               ! Length of an argument
      INTEGER LAST               ! Last line number in section body
      INTEGER LPACK              ! Length of the package name
      INTEGER NARG0              ! Initial argument count
      INTEGER NC                 ! Number of characters in buffer

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the routine prologue's name section.
      HEADER = 0
      NAME( 1 ) = 'Name'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If not found, then report an error.
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .NE. LAST ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SST_TRPAK_NAME',
     :   'Missing or invalid prologue "Name:" section.', STATUS )
         GO TO 99
      ENDIF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Use the name to start a routine definition for the package.
      CALL SST_PUT( 0, 'DEFINE ROUTINE "' //
     :                 SCB_LINE( FIRST )( SCB_FC( FIRST ) :
     :                                    SCB_LC( FIRST ) ) // '" -',
     :              STATUS )

*  Add a help library reference if required.
      IF ( HELP ) THEN
         CALL SST_PUT( 3, '/TOPIC_STRING = "' //
     :                 SCB_LINE( FIRST )( SCB_FC( FIRST ) :
     :                                    SCB_LC( FIRST ) ) // '" -',
     :                 STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Locate the routine prologue's purpose section.
      HEADER = 0
      NAME( 1 ) = 'Purpose'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If not found, then report an error.
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SST_TRPAK_PURP',
     :   'No prologue "Purpose:" section found.', STATUS )
         GO TO 99
      ENDIF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Form a routine description specification by concatenating the lines
*  in the purpose section.
      NC = 0
      DO 1 ILINE = FIRST, LAST
         IF ( ILINE .GT. FIRST ) CALL CHR_PUTC( ' ', BUF, NC )
         CALL CHR_PUTC( SCB_LINE( ILINE )( SCB_FC( ILINE ) :
     :                                     SCB_LC( ILINE ) ), BUF, NC )
1     CONTINUE

*  If the purpose description ends with a '.', then remove it.
      IF ( BUF( NC : NC ) .EQ. '.' ) THEN
         BUF( NC : NC ) = ' '
         NC = MAX( 1, CHR_LEN( BUF( : NC ) ) )
      END IF

*  Output the description specification.
      CALL SST_PUT( 3, '/DESCRIPTION = "' // BUF( : NC ) // '" -',
     :              STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Locate the routine prologue's invocation section.
      HEADER = 0
      NAME( 1 ) = 'Invocation'
      CALL SST_NSECT( .TRUE., 1, NAME, HEADER, FIRST, LAST, STATUS )

*  If not found, then report an error.
      IF ( ( HEADER .EQ. 0 ) .OR. ( FIRST .GT. LAST ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SST_TRPAK_INVK',
     :   'No prologue "Invocation:" section found.', STATUS )
         GO TO 99
      ENDIF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Concatenate the lines in the invocation section.
      NC = 0
      DO 2 ILINE = FIRST, LAST
         IF ( ILINE .GT. FIRST ) CALL CHR_PUTC( ' ', BUF, NC )
         CALL CHR_PUTC( SCB_LINE( ILINE )( SCB_FC( ILINE ) :
     :                                     SCB_LC( ILINE ) ), BUF, NC )
2     CONTINUE

*  Search the resulting invocation string for a parenthesised
*  expression (the argument list).
      CALL SST_FPARX( BUF( : NC ), F, L )

*  If one was found, and it is not empty, then find the first and last
*  argument characters.
      NARG0 = NARG
      IF ( F + 1 .LE. L - 1 ) THEN
         CALL CHR_FANDL( BUF( F + 1 : L - 1 ), FA, LA )

*  If the argument list is not all blank, then parse it to extract the
*  argument names and add them to the list in the ARG array.
         IF ( FA .LE. LA ) THEN
            FA = FA + F
            LA = LA + F
            CALL SST_PARGL( BUF( FA : LA ), MXARG, ARG, NARG, STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Determine the length of the package name and add the package name
*  specification to the routine definition.
      LPACK = MAX( 1, CHR_LEN( PACK ) )
      NC = 0
      CALL CHR_PUTC( '/PACKAGE = "', BUF, NC )
      CALL CHR_PUTC( PACK( : LPACK ), BUF, NC )
      CALL CHR_PUTC( '"', BUF, NC )
      IF ( NARG .GT. NARG0 ) CALL CHR_PUTC( ' -', BUF, NC )
      CALL SST_PUT( 3, BUF( : NC ), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If new arguments have been added to the argument list, then
*  concatenate them (with separating commas) in the text buffer.
      IF ( NARG .GT. NARG0 ) THEN
         NC = 0
         DO 3 I = NARG0 + 1, NARG
            IF ( I .GT. NARG0 + 1 ) CALL CHR_PUTC( ', ', BUF, NC )
            LARG = MAX( 1, CHR_LEN( ARG( I ) ) )
            CALL CHR_PUTC( ARG( I )( : LARG ), BUF, NC )
3        CONTINUE

*  Output the argument list.
         CALL SST_PUT( 3, BUF( : NC ), STATUS )
      END IF

*  Add a spacing line after the routine definition.
      CALL SST_PUT( 0, ' ', STATUS )

99    CONTINUE
      END
* @(#)sst_trpak.f   1.1   94/12/05 11:31:36   96/07/05 10:27:28
