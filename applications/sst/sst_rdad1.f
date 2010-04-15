      SUBROUTINE SST_RDAD1( NAME, TYPE, PURPOS, STATUS )
*+
*  Name:
*     SST_RDAD1

*  Purpose:
*     Read an "old style" ADAM prologue into the internal source code
*     buffer.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_RDAD1( STATUS )

*  Description:
*     The routine reads lines from the current input file and fills the
*     internal source code buffer with any "old style" ADAM prologue
*     lines encountered.  The routine will also extract the prologue
*     name and purpose description from the "begin prologue" line if
*     these can be found.  Only the first part of the prologue
*     (excluding declaration statements) is read. Processing terminates
*     when an "endhistory" line is read or when any ADAM prologue
*     section header which normally follows such a line is encountered.
*     Reading will also stop if an end of file occurs.
*
*     Any non-comment lines encountered in the prologue (or lines
*     encountered before a "begin prologue" line has been found) are
*     sent to the output file unchanged.  On successful exit, the
*     internal source code buffer contains the prologue lines, the
*     number of prologue lines read and the first and last character
*     positions in each line. Any previous contents of this buffer are
*     over-written.  The input file is left positioned to read the
*     remainder of the source code.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Returned)
*        Name of the routine in upper case, as extracted from the
*        prologue. A blank value is returned if the name cannot be
*        found.  If the result is truncated because the argument length
*        is insufficient, then an ellipsis '...' will be appended.
*     TYPE = CHARACTER * ( 1 ) (Returned)
*        The program unit type; one of 'B', 'F', 'P' or 'S', for block
*        data, function, program or subroutine. A value of '?' is
*        returned if the type cannot be determined.
*     PURPOS = CHARACTER * ( * ) (Returned)
*        "One line" description of the routine's purpose, as extracted
*        from the prologue. A blank value is returned if the
*        description cannot be found.  If the result is truncated
*        because the argument length is insufficient, then an ellipsis
*        '...' will be appended.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-FEB-1990 (RFWS):
*        Original, derived from the SST_RDPRO routine.
*     22-MAY-1990 (RFWS):
*        Changed to cope with "start of prologue" lines which do not
*        have the name and purpose aligned in the standard columns.
*     8-AUG-1990 (RFWS):
*        Improved the error reporting.
*     16-AUG-1990 (RFWS):
*        Changed to use SST_GET and SST_PUT instead of READ and WRITE.
*     7-SEP-1990 (RFWS):
*        Added TYPE argument.
*     28-SEP-1990 (RFWS):
*        Added calls to ERR_MARK and ERR_RLSE.
*     13-APR-2006 (TIMJ):
*        Seems that FIO__ENDFL is also a valid "end of file" status.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ constants
      INCLUDE 'FIO_PAR'          ! FIO_ public constants
      INCLUDE 'FIO_ERR'          ! FIO_ error codes

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST Source Code Buffer

*  Arguments Returned:
      CHARACTER * ( * ) NAME
      CHARACTER * ( 1 ) TYPE
      CHARACTER * ( * ) PURPOS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER * ( 32 ) PUNAME  ! Program unit name (dummy)
      CHARACTER * ( FIO__SZFNM ) FNAME ! File name
      CHARACTER * ( SST__SZLIN ) LINE ! Source line reading buffer
      INTEGER F                  ! First character position in string
      INTEGER ID                 ! Position of ' - ' character sequence
      INTEGER L                  ! Last character position in string
      INTEGER LSTAT              ! Local status variable
      INTEGER N                  ! Position of ellipsis
      INTEGER NC                 ! Number significant characters in line
      LOGICAL COMENT             ! Is line a comment line?
      LOGICAL HAVPUT             ! Has an output line been written?
      LOGICAL KPU                ! Whether program unit type is known
      LOGICAL PROLOG             ! Are we in the prologue?

*.

*  Clear the source code buffer.
      SCB_NLINE = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      NAME = ' '
      TYPE = '?'
      PURPOS = ' '
      KPU = .FALSE.
      PROLOG = .FALSE.
      HAVPUT = .FALSE.

*  Loop to read input lines, checking for errors.
      CALL ERR_MARK
1     CONTINUE
      CALL SST_GET( SCB_IN, LINE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Find the significant length of the line read and determine if it is
*  a comment line.
      NC = CHR_LEN( LINE )
      COMENT = INDEX( '*Cc', LINE( 1 : 1 ) ) .NE. 0

*  If the program unit type is not yet known and this line is not a
*  comment and is sufficiently long, then check to see if it is a
*  program unit declaration line from which the type can be obtained.
      IF ( ( .NOT. KPU ) .AND. ( .NOT. COMENT ) .AND.
     :     ( NC .GE. 7 ) ) THEN
         CALL SST_GTPUN( LINE( 7 : NC ), KPU, PUNAME, TYPE, STATUS )
      END IF

*  See if the line contains a "start of prologue" character sequence.
*  If so, then it should contain the routine name and purpose
*  description also. Search for the ' - ' character sequence that
*  separates these.
      IF ( ( .NOT. PROLOG ) .AND. COMENT .AND.
     :     ( LINE( 2 : 2 ) .EQ. '+' ) ) THEN
         PROLOG = .TRUE.
         ID = INDEX( LINE( : NC ), ' - ' )

*  If it is found in a valid position, then extract the name and
*  purpose values.
         IF ( ( ID .GE. 4 ) .AND. ( ID .LE. NC - 3 ) ) THEN

*  Find the first and last purpose characters and copy them to the
*  output argument, appending a '.' if there is not one already.
            CALL CHR_FANDL( LINE( ID + 3 : NC ), F, L )
            F = F + ID + 2
            IF ( LINE( NC : NC ) .EQ. '.' ) THEN
               CALL CHR_COPY( LINE( F : NC ), .FALSE., PURPOS, LSTAT )
            ELSE
               CALL CHR_COPY( LINE( F : NC ) // '.', .FALSE., PURPOS,
     :                        LSTAT )
            END IF

*  If truncation occurred, then append an ellipsis '...'.
            IF ( LSTAT .NE. 0 ) THEN
               N = MAX( 1, LEN( PURPOS ) - 2 )
               PURPOS( N : ) = '...'
            END IF

*  Find the first and last name characters and extract the name.
            CALL CHR_FANDL( LINE( 3 : ID ), F, L )
            IF ( F .LE. L ) THEN
               F = F + 2
               L = L + 2
               CALL CHR_COPY( LINE( F : L ), .FALSE., NAME, LSTAT )

*  If truncation occurred, then append an ellipsis '...'.
               IF ( LSTAT .NE. 0 ) THEN
                  N = MAX( 1, LEN( NAME ) - 2 )
                  NAME( N : ) = '...'
               END IF

*  Ensure the returned name is in upper case.
               CALL CHR_UCASE( NAME )
            END IF
         END IF

*  Any non-comment lines or lines encountered before the prologue
*  begins are sent straight to the output file. Blank output lines are
*  supressed until at least one non-blank line has been output.
      ELSE IF ( ( .NOT. PROLOG ) .OR. ( .NOT. COMENT ) ) THEN
         IF ( NC .EQ. 0 ) THEN
            IF ( HAVPUT ) CALL SST_PUT( 0, ' ', STATUS )
         ELSE
            CALL CHR_FANDL( LINE( : NC ), F, L )
            CALL SST_PUT( F - 1, LINE( F : L ), STATUS )
            HAVPUT = .TRUE.
         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  See if this is an "end history" prologue line. If so, then it marks
*  the end of the first part of the prologue in which this routine is
*  interested.  Disregard this line and exit from the reading loop.
      ELSE
         IF ( LINE( : NC ) .EQ. '*    endhistory' ) THEN
            GO TO 99

*  If a prologue header which appears after "endhistory" is
*  encountered, then this is probably because the "endhistory" line has
*  been deleted.  In this case we have read too far, so back space the
*  input file and exit from the reading loop. (Note this test is broken
*  up to keep the number of continuation lines down.)
         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Type Definitions :' ) .OR.
     :             CHR_SIMLR( LINE( : NC ),
     :                        '*    Global constants :' ) .OR.
     :             CHR_SIMLR( LINE( : NC ),
     :                        '*    Import :' ) .OR.
     :             CHR_SIMLR( LINE( : NC ),
     :                        '*    Import-Export :' ) .OR.
     :             CHR_SIMLR( LINE( : NC ),
     :                        '*    Export :' ) ) THEN
            BACKSPACE( SCB_IN )
            GO TO 99

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Status :' ) .OR.
     :             CHR_SIMLR( LINE( : NC ),
     :                        '*    External references :' ) .OR.
     :             CHR_SIMLR( LINE( : NC ),
     :                        '*    Local Constants :' ) .OR.
     :             CHR_SIMLR( LINE( : NC ),
     :                        '*    Local variables :' ) .OR.
     :             CHR_SIMLR( LINE( : NC ),
     :                        '*    Global variables :' ) ) THEN
            BACKSPACE( SCB_IN )
            GO TO 99

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Internal References :' ) .OR.
     :             CHR_SIMLR( LINE( : NC ),
     :                        '*    Local data :' ) .OR.
     :             ( LINE ( : 2 ) .EQ. '*-' ) ) THEN
            BACKSPACE( SCB_IN )
            GO TO 99

*  Otherwise, this is a normal prologue line. Check that the internal
*  source code buffer will not overflow if this line is added. Report
*  an error if it will.
         ELSE
            IF ( SCB_NLINE .GE. SST__MXLIN ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'MXLIN', SST__MXLIN )
               FNAME = '?'
               INQUIRE( UNIT = SCB_IN, NAME = FNAME )
               CALL MSG_SETC( 'FILE', FNAME )
               CALL ERR_REP( 'SST_RDAD1_XS',
     :         'More than ^MXLIN prologue lines read from file ' //
     :         '^FILE - internal source code buffer overflow.', STATUS )
               GO TO 99

*  Remove any non-printing characters and "old style" ADAM
*  placeholders. Then remove the leading comment character.
            ELSE
               CALL CHR_CLEAN( LINE( : NC ) )
               CALL SST_ZAPAP( LINE( : NC ), STATUS )
               LINE( 1 : 1 ) = ' '

*  Put the line into the internal source code buffer.
               SCB_NLINE = SCB_NLINE + 1
               SCB_LINE( SCB_NLINE ) = LINE( : NC )

*  Find the first and last non-blank character positions and store
*  them.
               CALL CHR_FANDL( SCB_LINE( SCB_NLINE )( : NC ),
     :                         SCB_FC( SCB_NLINE ), SCB_LC( SCB_NLINE) )
            END IF
         END IF
      END IF

*  Return to read the next prologue line.
      GO TO 1

*  Arrive here if an end of prologue line was read, or an error was
*  encountered. Annul end-of-file errors.
99    CONTINUE
      IF ( STATUS .EQ. FIO__EOF
     :     .OR. STATUS .EQ. FIO__ENDFL) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

      END
* @(#)sst_rdad1.f   1.1   94/12/05 11:31:32   96/07/05 10:27:26
