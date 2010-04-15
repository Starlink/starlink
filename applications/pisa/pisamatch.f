      SUBROUTINE PISAMATCH( STATUS )
*+
*  Name:
*     PISAMATCH

*  Purpose:
*     Matches the indices in one file against those in a second file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISAMATCH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine matches the first column of values (the object
*     indices) of the two input files. When a match is located between
*     the indices the complete entry in the second file is copied to the
*     output file. It is intended primarily to match a list of object
*     indices (such as those produced by PISAKNN or possibly a PISACUT
*     list of PISAPEAK results).

*  Usage:
*     PISAMATCH ONE TWO OUT

*  ADAM Parameters:
*     ONE = FILENAME (Read)
*        Name of the file containing the indices to match to those of
*        file TWO.
*     TWO = FILENAME (Read)
*        Name of the file containing the entries whose indices are to be
*        matched to those of file ONE. Entries with a matched index will
*        be written to file OUT.
*     OUT = FILENAME (Write)
*        Name of a file to contain the entries from file TWO which have
*        the same indices as those specified in file ONE.
*        [PISAMATCH.DAT]

*  Examples:
*     PISAMATCH CLASS1 PISAFIND STARS
*        This matches the indices in file CLASS1 to those of file
*        PISAFIND. The PISAFIND entries with indices found in file
*        CLASS1 are written to file STARS.

*  Notes:
*     -  The maximum number of entries allowed in file ONE is 10000. No
*     restriction applies to file TWO.
*     -  The indices in both files must be in the first column.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAR-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system status constants
      INCLUDE 'FIO_PAR'          ! FIO parameters


*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing
                                 ! blanks

*  Local Constants:
      INTEGER MAXLIN             ! Maximum number of input lines in data
                                 ! files
      PARAMETER ( MAXLIN = 10000 ) ! Needs implementing dynamically
                                   ! when PSX arrives.

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME1 ! Name of file
      CHARACTER * ( FIO__SZFNM ) FNAME2 ! Name of file
      CHARACTER * ( 256 ) LINE   ! line of data from file
      INTEGER NUMBS( MAXLIN )    ! indices of input files
      INTEGER I                  ! number of entries in first file
      INTEGER IFS1,
     :        IFS2,
     :        IFS3               ! FIO system file descriptors
      LOGICAL OPNF1,             ! set when corresponding files are
     :        OPNF2,             ! open
     :        OPNF3
      INTEGER NCHAR              ! dummy
      INTEGER ISTART,            ! positions of start and end of integer
     :        IEND               ! character string ( first word )
      INTEGER K                  ! loop variable
      INTEGER INDEX              ! current index value of second file
      INTEGER IMATCH             ! number of matched entries
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the input files.
      CALL PSA1_ASFIO( 'ONE', 'READ', 'LIST', 0, IFS1, OPNF1,
     :                 STATUS )
      CALL FIO_FNAME( IFS1, FNAME1, STATUS )

*  The second file containing the interesting information.
      CALL PSA1_ASFIO( 'TWO', 'READ', 'LIST', 0, IFS2, OPNF2,
     :                 STATUS )
      CALL FIO_FNAME( IFS2, FNAME2, STATUS )

*  Read in indices from the first file.
      I = 0
3     CONTINUE
         CALL FIO_READ( IFS1, LINE, NCHAR, STATUS )
         IF ( STATUS .EQ. FIO__EOF) THEN
            CALL ERR_ANNUL( STATUS )
            GO TO 4
         END IF
         I = I + 1

*  Decode the line reading in first number.
         ISTART = 0
         CALL CHR_FIWS( LINE, ISTART, STATUS )
         IEND = ISTART
         CALL CHR_FIWE( LINE, IEND, STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FNAME', FNAME1 )
            CALL ERR_REP( 'BADENTRY',
     :      'File ^FNAME1 contains invalid entries'//
     :      '( blank or no lines )', STATUS )
            GO TO 99
         END IF

*  Convert the string into an integer.
         CALL CHR_CTOI( LINE( ISTART : IEND ), NUMBS( I ),
     :                  STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FNAME', FNAME1 )
            CALL ERR_REP( 'BADVALUE',
     :      'File ^FNAME contains an invalid index', STATUS )
             GO TO 99
         END IF
      GO TO 3
4     CONTINUE

*  Access an output file to contain the matched entries.
5     CONTINUE
      CALL PSA1_ASFIO( 'OUT', 'WRITE', 'LIST', 0, IFS3, OPNF3,
     :                 STATUS )

*  Extract the appropriate lines from the base file.
      IMATCH = 0
6     CONTINUE
         CALL FIO_READ( IFS2, LINE, NCHAR, STATUS )
         IF ( STATUS .EQ. FIO__EOF) THEN
            CALL ERR_ANNUL( STATUS )
            GO TO 9
         END IF

*  Decode the line reading in first number.
         ISTART = 0
         CALL CHR_FIWS( LINE, ISTART, STATUS )
         IEND = ISTART
         CALL CHR_FIWE( LINE, IEND, STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FNAME', FNAME2 )
            CALL ERR_REP( 'BADENTRY2',
     :      'File ^FNAME contains invalid entries'//
     :      '( blank or no lines )', STATUS )
            GO TO 99
         END IF

*  Convert the string into an integer.
         CALL CHR_CTOI( LINE( ISTART : IEND ), INDEX,
     :                  STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FNAME', FNAME2 )
            CALL ERR_REP( 'BADVALUE2',
     :      'File ^FNAME contains an invalid index', STATUS )
             GO TO 99
         END IF

*  Now look through the file one indices looking for a match.
         DO 7 K = 1, I
            IF( INDEX .EQ. NUMBS( K ) ) THEN

* Got a line transfer it and break out, for next line
               CALL FIO_WRITE( IFS3, LINE( :CHR_LEN( LINE ) ), STATUS )
               IMATCH = IMATCH + 1
               GO TO 8
            ENDIF
7        CONTINUE
8     CONTINUE
      GO TO 6

*  End of file break.
9     CONTINUE

*  Write out some information on the match statistics.
      CALL FIO_FNAME( IFS3, FNAME1, STATUS )
      CALL MSG_SETC( 'FNAME', FNAME1 )
      CALL MSG_SETI( 'MATCHED', IMATCH )
      CALL MSG_OUT( 'OUTPUT_MESSAGE',
     : ' ^MATCHED entries written to file ^FNAME', STATUS )

*  Error label.
99    CONTINUE

*  Close the files.
      IF( OPNF1 ) THEN
         CALL FIO_CLOSE( IFS1, STATUS )
         CALL PAR_CANCL( 'ONE', STATUS )
      END IF
      IF( OPNF2 ) THEN
         CALL FIO_CLOSE( IFS2, STATUS )
         CALL PAR_CANCL( 'TWO', STATUS )
      END IF
      IF( OPNF3 ) THEN
         CALL FIO_CLOSE( IFS3, STATUS )
         CALL PAR_CANCL( 'OUT', STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISAMATCH_ERR',
     :   'PISAMATCH: Error matching the list indices.',
     :   STATUS )
      END IF

      END
* $Id$
