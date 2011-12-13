      SUBROUTINE SST_RDAD2( STATUS )
*+
*  Name:
*     SST_RDAD2

*  Purpose:
*     Transfer the 2nd part of an "old style" ADAM prologue into the
*     output file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_RDAD2( STATUS )

*  Description:
*     The routine reads lines from the current input file and transfers
*     them to the output file with any "old style" ADAM prologue
*     section headings translated into their STARLSE equivalents.  It
*     is intended to read only the second half of a prologue (starting
*     at the declarations), to follow on from where the SST_RDAD1
*     routine leaves off.  Data transfer finishes when a new "begin
*     prologue" line is read; the file is left positioned ready to
*     re-read this line.  Data transfer will also stop if an end of
*     file is encountered.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990, 1994 Science & Engineering Research Council.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
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
*     1-MAR-1990 (RFWS):
*        Original, derived from the SST_RDAD1 routine.
*     16-AUG-1990 (RFWS):
*        Changed to use SST_GET and SST_PUT instead of READ and WRITE.
*     16-AUG-1990 (RFWS):
*        Improved the handling of blank lines in prologues.
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
      INCLUDE 'SST_PAR'          ! SST_ constants
      INCLUDE 'FIO_PAR'          ! FIO_ public constants
      INCLUDE 'FIO_ERR'          ! FIO_ error codes

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST Source Code Buffer

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      CHARACTER * ( SST__SZLIN ) LINE ! Source line reading buffer
      INTEGER F                  ! First character position in string
      INTEGER L                  ! Last character position in string
      INTEGER NC                 ! Number significant characters in line
      LOGICAL LRECOG             ! Last line had recognised header?
      LOGICAL PREVBL             ! Previous output line blank?
      LOGICAL PROLOG             ! Are we in the prologue?
      LOGICAL RECOG              ! Line has recognised header?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      PROLOG = .TRUE.
      LRECOG = .FALSE.
      PREVBL = .TRUE.

*  Loop to read input lines, checking for errors.
      CALL ERR_MARK
1     CONTINUE
      CALL SST_GET( SCB_IN, LINE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Find the significant length of the line read.
      NC = CHR_LEN( LINE )

*  If we are not in a prologue, then see if the line contains a "start
*  of prologue" character sequence. If so, then back space the input
*  file and terminate the input loop.
      IF ( .NOT. PROLOG ) THEN
         IF ( ( INDEX( '*Cc', LINE( 1 : 1 ) ) .NE. 0 ) .AND.
     :        ( LINE( 2 : 2 ) .EQ. '+' ) ) THEN
            BACKSPACE( SCB_IN )
            GO TO 99

*  Otherwise, non-prologue lines are simply transferred to the output
*  file.
         ELSE
            IF ( NC .EQ. 0 ) THEN
               CALL SST_PUT( 0, ' ', STATUS )
            ELSE
               CALL CHR_FANDL( LINE( : NC ), F, L )
               CALL SST_PUT( F - 1, LINE( F : L ), STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 99
         END IF

*  If we are in a prologue, then blank lines are sent straight to the
*  output file, unless they follow a recognised prologue header line or
*  the previous line was also blank.
      ELSE
         RECOG = .TRUE.
         IF ( NC .EQ. 0 ) THEN
            IF ( .NOT. ( LRECOG .OR. PREVBL ) ) THEN
               CALL SST_PUT( 0, ' ', STATUS )
               PREVBL = .TRUE.
               IF ( STATUS .NE. SAI__OK ) GO TO 99
            END IF

*  See if the input line contains an "end of prologue" character
*  sequence.  If so, then write the equivalent output sequence and turn
*  the prologue flag off.
         ELSE IF ( ( INDEX( '*Cc', LINE( 1 : 1 ) ) .NE. 0 ) .AND.
     :             ( LINE( 2 : 2 ) .EQ. '-' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 1, '.', STATUS )
            CALL SST_PUT ( 0, ' ', STATUS )
            PROLOG = .FALSE.
            RECOG = .FALSE.
            PREVBL = .TRUE.

*  Any non-comment lines are sent straight to the output file. However,
*  blank lines are only used if the previous line was not blank.
         ELSE IF ( INDEX( '*Cc', LINE( 1 : 1 ) ) .EQ. 0 ) THEN
            RECOG = .FALSE.
            IF ( NC .EQ. 0 ) THEN
               IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
               PREVBL = .TRUE.
            ELSE
               CALL CHR_FANDL( LINE( : NC ), F, L )
               CALL SST_PUT( F - 1, LINE( F : L ), STATUS )
               PREVBL = .FALSE.
            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If a recognised prologue header is encountered, then write out the
*  translated equivalent (preceded by a blank line if the previous line
*  was not blank).
         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Type Definitions :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Type Definitions:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Global constants :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Global Constants:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Import :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Arguments Given:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Import-Export :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Arguments Given and Returned:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Export :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Arguments Returned:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Status :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Status:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    External references :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'External References:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Local Constants :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Local Constants:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Local variables :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Local Variables:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Global variables :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Global Variables:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Internal References :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Internal References:', STATUS )

         ELSE IF ( CHR_SIMLR( LINE( : NC ),
     :                        '*    Local data :' ) ) THEN
            IF ( .NOT. PREVBL ) CALL SST_PUT( 0, ' ', STATUS )
            CALL SST_FOR( 3, 'Local Data:', STATUS )

*  Otherwise, send the line to the output file unchanged.
         ELSE
            RECOG = .FALSE.
            CALL CHR_FANDL( LINE( : NC ), F, L )
            CALL SST_PUT( F - 1, LINE( F : L ), STATUS )
            PREVBL = .FALSE.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
         END IF
      END IF

*  Note that the previous line could not have been blank if it was a
*  recognised prologue header line.
      IF ( RECOG ) PREVBL = .FALSE.

*  Return to read the next input line, noting whether the previous one
*  was recognised.
      LRECOG = RECOG
      GO TO 1

*  Arrive here if a start of prologue line is read, or if an error is
*  encountered. Annul end-of-file errors.
99    CONTINUE
      IF ( STATUS .EQ. FIO__EOF .OR.
     :     STATUS .EQ. FIO__ENDFL ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

      END
* @(#)sst_rdad2.f   1.1   94/12/05 11:31:33   96/07/05 10:27:27
