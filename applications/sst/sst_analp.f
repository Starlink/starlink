      SUBROUTINE SST_ANALP( PUNAME, PUTYPE, MXCODE, NBLANK, NCODBL,
     :                      NCODE, NCODEU, NCOM, NCOMBL, NLINE, NPROL,
     :                      NPROLB, NPROLE, NCCHR, EOF, EFFCOM, STATUS )
*+
*  Name:
*     SST_ANALP

*  Purpose:
*     Analyse a program unit, counting lines, characters, etc.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_ANALP( PUNAME, PUTYPE, MXCODE, NBLANK, NCODBL,
*                     NCODE, NCODEU, NCOM, NCOMBL, NLINE, NPROL,
*                     NPROLB, NPROLE, NCCHR, EOF, EFFCOM, STATUS )

*  Description:
*     This routine reads a program unit from the input file and
*     analyses it. This involves determining its name and type
*     (subroutine, function, etc.) and counting the number of comment
*     lines, code lines, characters, etc. Statistics derived from these
*     counts are returned. The end of a program unit is determined by
*     the occurence of an 'END' statement, or by an end-of-file
*     condition; a flag is returned to indicate which.

*  Arguments:
*     PUNAME = CHARACTER * ( * ) (Returned)
*        Name of the program unit (in upper case). A value of
*        '<unknown>' will be returned if the name cannot be determined.
*     PUTYPE = CHARACTER * ( 1 ) (Returned)
*        The program unit type ('B', 'F', 'P' or 'S'). A value of '?'
*        is returned if the type cannot be determined.
*     MXCODE = INTEGER (Returned)
*        The maximum number of contiguous (apart from intervening blank
*        lines) uncommented code lines found in the program unit.
*     NBLANK = INTEGER (Returned)
*        Number of blank lines found (including lines containing only a
*        single comment character).
*     NCODBL = INTEGER (Returned)
*        Number of blocks of contiguous (apart from intervening blank
*        lines) uncommented code lines found.
*     NCODE = INTEGER (Returned)
*        Number of non-blank code lines found.
*     NCODEU = INTEGER (Returned)
*        Number of non-blank code lines found which also do not have an
*        end-of-line comment.
*     NCOM = INTEGER (Returned)
*        Number of non-blank comment lines found in the program unit
*        (i.e. excluding all those which contain only a single comment
*        character). End-of-line comments are not included.
*     NCOMBL = INTEGER (Returned)
*        Number of blocks of contiguous comments found (i.e. not
*        separated by code lines) within the code. Prologue comment
*        lines, prologue delimiting lines and end-of-line comments do
*        not contribute to this count.
*     NLINE = INTEGER (Returned)
*        Total number of lines read from the file for the current
*        program unit.
*     NPROL = INTEGER (Returned)
*        Number of non-blank comment lines found in prologues (i.e.
*        excluding all those which contain only a single comment
*        character).
*     NPROLB = INTEGER (Returned)
*        Number of begin-prologue lines found.
*     NPROLE = INTEGER (Returned)
*        Number of end-prologue lines found.
*     NCCHR = INTEGER (Returned)
*        Total number of alphanumeric characters found in comment lines
*        within the program unit, excluding end-of-line comments and
*        those in the prologue.
*     EOF = LOGICAL (Returned)
*        Whether the program unit was terminated by an end-of-file
*        condition.
*     EFFCOM = REAL (Returned)
*        The effective number of in-code comments in the program unit,
*        after allowing for the number of alphanumeric characters in
*        each comment line (a full comment is considered to have 50
*        such characters and those which fall short are counted as
*        fractional lines). End-of-line comments are included in this
*        value, but comments in the prologue are not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     4-SEP-1990 (RFWS):
*        Original version.
*     6-SEP-1990 (RFWS):
*        Fixed bug causing the last set of contiguous lines to be
*        ignored.
*     6-SEP-1990 (RFWS):
*        Improved the distinction between in-code comment blocks and
*        prologue comment blocks.
*     28-SEP-1990 (RFWS):
*        Changed to take account of code lines which contain
*        end-of-line comments, and to return a separate count of code
*        lines which do not have such comments.
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
      INCLUDE 'FIO_ERR'          ! FIO_ error codes

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST Source Code Buffer

*  Arguments Returned:
      CHARACTER * ( * ) PUNAME
      CHARACTER * ( 1 ) PUTYPE
      INTEGER MXCODE
      INTEGER NBLANK
      INTEGER NCODBL
      INTEGER NCODE
      INTEGER NCODEU
      INTEGER NCOM
      INTEGER NCOMBL
      INTEGER NLINE
      INTEGER NPROL
      INTEGER NPROLB
      INTEGER NPROLE
      INTEGER NCCHR
      LOGICAL EOF
      REAL EFFCOM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      REAL FULLCC                ! Characters in a full comment line
      PARAMETER ( FULLCC = 50.0 )

*  Local Variables:
      CHARACTER * ( SST__SZLIN ) LINE ! Input buffer
      INTEGER IHIC               ! Position of e-o-l comment character
      INTEGER NC                 ! Number of input characters
      INTEGER NCH                ! Number of alphanumeric characters
      INTEGER NCTCOD             ! Number of contiguous code lines
      LOGICAL BLANK              ! Line is blank?
      LOGICAL COMENT             ! Line is a comment?
      LOGICAL INCODE             ! Whether in a block of code
      LOGICAL INCOMM             ! Whether in a block of comments
      LOGICAL ISEND              ! Whether END statement found
      LOGICAL KPUNAM             ! Program unit name known?
      LOGICAL PROLOG             ! Whether in a prologue
      LOGICAL WASCOD             ! Previously in a code block?
      LOGICAL WASCOM             ! Previously in a comment block?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise before accumulating statistics.
      EFFCOM = 0.0
      EOF = .FALSE.
      INCODE = .FALSE.
      INCOMM = .FALSE.
      ISEND = .FALSE.
      KPUNAM = .FALSE.
      MXCODE = 0
      NBLANK = 0
      NCCHR = 0
      NCODBL = 0
      NCODE = 0
      NCODEU = 0
      NCOM = 0
      NCOMBL = 0
      NCTCOD = 0
      NLINE = 0
      NPROL = 0
      NPROLB = 0
      NPROLE = 0
      PROLOG = .FALSE.
      PUNAME = '<unknown>'
      PUTYPE = '?'
      WASCOD = .FALSE.
      WASCOM = .FALSE.

*  Loop to read the input code lines for the next program unit. The end
*  of this unit is determined by the occurrence of a Fortran END
*  statement or the end of the input file.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( .NOT. ( ISEND .OR. EOF ) ) THEN

*  Read a line from the input file, checking for end of file and
*  annulling the associated error condition.
         CALL ERR_MARK
         CALL SST_GET( SCB_IN, LINE, STATUS )
         IF ( STATUS .EQ. FIO__EOF .OR.
     :        STATUS .EQ. FIO__ENDFL) THEN
            EOF = .TRUE.
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If a line was read (i.e. the end of the input file has not been
*  reached), then count it, find its length and remove any
*  non-printable characters (this prevents blank lines appearing to be
*  non-blank because of the presence of TAB characters, for instance).
         IF ( .NOT. EOF ) THEN
            NLINE = NLINE + 1
            NC = MAX( 2, CHR_LEN( LINE ) )
            CALL CHR_CLEAN( LINE ( : NC ) )

*  Note if it is a comment line.
            COMENT = INDEX( '*Cc', LINE( 1 : 1 ) ) .NE. 0

*  Note if it is blank (whether a comment or not).
            BLANK = LINE( 2 : NC ) .EQ. ' '
            IF ( .NOT. COMENT ) BLANK = BLANK .AND.
     :                                  ( LINE( 1 : 1 ) .EQ. ' ' )

*  Count the number of blank lines encountered.
            IF ( BLANK ) THEN
               NBLANK = NBLANK + 1

*  Count the total number of (non-blank) comment lines. When such a
*  line is found, note that we are no longer in a block of code lines.
*  Initially, assume we are not in a block of in-code comments either
*  (until we establish if we are in a prologue or not).
            ELSE IF ( COMENT ) THEN
               NCOM = NCOM + 1
               WASCOD = .FALSE.
               WASCOM = .FALSE.

*  If the comment line is a prologue delimiting line, then toggle the
*  PROLOG flag and count either one prologue END line, or one prologue
*  BEGIN line, as appropriate.
               IF ( INDEX( '+-', LINE( 2 : 2 ) ) .NE. 0 ) THEN
                  IF ( PROLOG ) THEN
                     PROLOG = .FALSE.
                     NPROLE = NPROLE + 1
                  ELSE
                     PROLOG = .TRUE.
                     NPROLB = NPROLB + 1
                  END IF

*  Count the number of (non-blank) comment lines which lie within
*  prologues. This count does not include prologue delimiting lines.
               ELSE IF ( PROLOG ) then
                  NPROL = NPROL + 1

*  If the current line is a comment lying outside the prologue (and is
*  not a prologue delimiting line), then we are now in a block of
*  in-code comments.
               ELSE
                  WASCOM = .TRUE.

*  Count the number of alphanumeric characters in the line as a simple
*  measure of the comment information it contains.
                  CALL SST_CNTAC( LINE( 2 : NC ), NCH )

*  Accumulate sums for estimating the final effective number of
*  comments, assigning reduced weight to lines which do not contain a
*  full complement of alphanumeric characters.
                  NCCHR = NCCHR + NCH
                  EFFCOM = EFFCOM + MIN( 1.0, REAL( NCH ) / FULLCC )
               END IF

*  If the current line is not blank and is not a comment, then it must
*  be code, so count it. When such a line is found, note that we are no
*  longer in a block of comments. Initially, assume we are in a block
*  of uncommented code (until we establish if an end-of-line comment is
*  present).
            ELSE
               NCODE = NCODE + 1
               WASCOM = .FALSE.
               WASCOD = .TRUE.

*  Determine if there is an end-of-line comment character in the code
*  line followed by at least one character of comment information.  If
*  not, then count this line as being completely uncommented.
               CALL SST_HIC( LINE( : NC ), IHIC )
               IF ( ( IHIC .LE. 0 ) .OR. ( IHIC .GE. NC ) ) THEN
                  NCODEU = NCODEU + 1

*  Increment the count of contiguous uncommented code lines.
                  NCTCOD = NCTCOD + 1

*  If there is an end-of-line comment, then we are not in a block of
*  uncommented code.
               ELSE
                  WASCOD = .FALSE.

*  Count the number of alphanumeric characters in end-of-line comments
*  as a simple measure of the information they contain.
                  CALL SST_CNTAC( LINE( IHIC + 1 : NC ), NCH )

*  Accumulate sums for estimating the final effective number of
*  comments, assigning reduced weight to lines which do not contain a
*  full complement of alphanumeric characters.
                  EFFCOM = EFFCOM + MIN( 1.0, REAL( NCH ) / FULLCC )
               END IF

*  If the current program unit name is not known, and the line is long
*  enough, then see if the name can be obtained from it.
               IF ( ( .NOT. KPUNAM ) .AND. ( NC .GE. 7 ) ) THEN
                  CALL SST_GTPUN( LINE( 7 : NC ), KPUNAM, PUNAME,
     :                            PUTYPE, STATUS )
               END IF

*  If the line is long enough, then see if it contains an END statement
*  which terminates the current program unit.
               IF ( NC .GE. 7 ) THEN
                  CALL SST_GTEND( LINE( 7 : NC ), ISEND, STATUS )
               END IF
            ENDIF
         END IF

*  If we were previously in a block of code lines, but the last line
*  was not in such a block, then increment the sum for forming the mean
*  code block length and note the maximum length.  Reset the count of
*  contiguous code lines.
         IF ( INCODE .AND. ( .NOT. WASCOD ) ) THEN
            NCODBL = NCODBL + 1
            MXCODE = MAX( MXCODE, NCTCOD )
            NCTCOD = 0

*  If we were previously in a block of comment lines, but the last line
*  was not in such a block, then increment the sum for forming the mean
*  comment block length.
         ELSE IF ( INCOMM .AND. ( .NOT. WASCOM ) ) THEN
            NCOMBL = NCOMBL + 1
         END IF

*  Note if we are now in a block of comment or code lines.
         INCOMM = WASCOM
         INCODE = WASCOD

*  End of loop for reading all the lines in a program unit.
         GO TO 1
      END IF

*  After reading the last line in a program unit, ensure that the last
*  set of contiguous line information is used (in the same way as is
*  done above after each line).
         IF ( INCODE ) THEN
            NCODBL = NCODBL + 1
            MXCODE = MAX( MXCODE, NCTCOD )
            NCTCOD = 0
         ELSE IF ( INCOMM ) THEN
            NCOMBL = NCOMBL + 1
         END IF

*  Arrive here if an error occurs.
99    CONTINUE

      END
* @(#)sst_analp.f   1.1   94/12/05 11:31:21   96/07/05 10:27:31
