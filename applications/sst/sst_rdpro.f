      SUBROUTINE SST_RDPRO( STATUS )
*+
*  Name:
*     SST_RDPRO

*  Purpose:
*     Read the input file prologue into the internal source code buffer.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_RDPRO( STATUS )

*  Description:
*     The routine reads lines from the current input file until a
*     "begin prologue" delimiting line (e.g. '*+') is found. Subsequent
*     comment lines are then read into the internal source code buffer
*     (with their comment characters replaced by blanks) until an "end
*     prologue" delimiting line (e.g. '*-') is encountered. Reading
*     then stops. Reading will also stop if an end of file is
*     encountered.  On successful exit, the internal source code buffer
*     also contains the number of prologue lines read and the first and
*     last character positions in each line. Any previous contents of
*     this buffer are over-written. The input file is left positioned
*     following the last read operation (i.e. ready to find the next
*     prologue if it exists).

*  Notes:
*     -  Any non-printing characters in prologue lines are converted to
*     blanks.
*     -  Those STARLSE placeholders which are intended to be left in
*     place in prologues are also detected and replaced with blanks.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-DEC-1989 (RFWS):
*        Original version.
*     22-DEC-1989 (RFWS):
*        Added call to remove STARLSE placeholders from prologue lines.
*     13-AUG-1990 (RFWS):
*        Changed to exit without error on end-of-file.
*     13-AUG-1990 (RFWS):
*        Fixed bug whereby completely blank lines were ignored, causing
*        new paragraphs to be missed.
*     14-AUG-1990 (RFWS):
*        Changed to use SST_GET instead of READ.
*     28-SEP-1990 (RFWS):
*        Added calls to ERR_MARK and ERR_RLSE.
*     13-APR-2006 (TIMJ):
*        Check for ENDFL as well as EOF
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ constsnts
      INCLUDE 'FIO_PAR'          ! FIO_ public constants
      INCLUDE 'FIO_ERR'          ! FIO_ error codes

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST_ source code buffer

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( 2 ) TEST     ! Test characters for start of prologue
      CHARACTER * ( FIO__SZFNM ) FNAME ! File name
      CHARACTER * ( SST__SZLIN ) LINE ! Source line reading buffer
      INTEGER NC                 ! Number significant characters in line

*.

*  Clear the source code buffer.
      SCB_NLINE = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop to read input lines, checking for errors.
      CALL ERR_MARK
1     CONTINUE                   ! Start of 'DO WHILE' loop
      CALL SST_GET( SCB_IN, TEST, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Keep reading until a comment line is found with '+' in the second
*  column. This line is not wanted, but marks the start of the
*  prologue.
      IF ( ( TEST( 2 : 2 ) .NE. '+' ) .OR.
     :     ( INDEX( '*Cc', TEST( 1 : 1 ) ) .EQ. 0 ) ) GO TO 1

*  Loop to read the prologue lines, checking for errors.
2     CONTINUE
      CALL SST_GET( SCB_IN, LINE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Test if the line is a comment line. Ignore those that aren't, unless
*  they are completely blank (these count as blank comment lines even
*  if they do not actually have a comment character).
      NC = CHR_LEN( LINE )
      IF ( ( NC .EQ. 0 ) .OR.
     :     ( INDEX( '*Cc', LINE( 1 : 1 ) ) .NE. 0 ) ) THEN

*  If there is a '-' in the second column, then it marks the end of the
*  prologue. Disregard this line and exit from the reading loop.
         IF ( LINE( 2 : 2 ) .EQ. '-' ) THEN
            GO TO 99

*  Check that the internal source code buffer will not overflow if this
*  line is added. Report an error if it will.
         ELSE
            IF ( SCB_NLINE .GE. SST__MXLIN ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'MXLIN', SST__MXLIN )
               FNAME = '?'
               INQUIRE( UNIT = SCB_IN, NAME = FNAME )
               CALL MSG_SETC( 'FILE', FNAME )
               CALL ERR_REP( 'SST_RDPRO_XS',
     :         'More than ^MXLIN prologue lines read from file ' //
     :         '^FILE - internal source code buffer overflow.',
     :         STATUS )

*  If the line is not blank, remove any non-printing characters and
*  STARLSE placeholders, then remove the leading comment character.
            ELSE
               IF ( NC .GT. 0 ) THEN
                  CALL CHR_CLEAN( LINE( : NC ) )
                  CALL SST_ZAPPL( LINE( : NC ), STATUS )
                  LINE( 1 : 1 ) = ' '

*  Put the line into the internal source code buffer.
                  SCB_NLINE = SCB_NLINE + 1
                  SCB_LINE( SCB_NLINE ) = LINE( : NC )

*  Find the first and last non-blank character positions and store
*  them.
                  CALL CHR_FANDL( SCB_LINE( SCB_NLINE )( : NC ),
     :                            SCB_FC( SCB_NLINE ),
     :                            SCB_LC( SCB_NLINE) )


*  If the input line is blank, then insert a blank line into the source
*  code buffer.
               ELSE
                  SCB_NLINE = SCB_NLINE + 1
                  SCB_LINE( SCB_NLINE ) = ' '
                  SCB_FC( SCB_NLINE ) = 1
                  SCB_LC( SCB_NLINE ) = 0
               END IF
            END IF
         END IF
      END IF

*  Return to read the next prologue line.
      GO TO 2

*  Arrive here if an error occurred. Annul end-of-file errors.
99    CONTINUE
      IF ( STATUS .EQ. FIO__EOF .OR.
     :     STATUS .EQ. FIO__ENDFL) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

      END
* @(#)sst_rdpro.f   1.1   94/12/05 11:31:33   96/07/05 10:27:28
