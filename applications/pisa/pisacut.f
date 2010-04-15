      SUBROUTINE PISACUT( STATUS )
*+
*  Name:
*     PISACUT

*  Purpose:
*     PISACUT separates a formatted file into two parts.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISACUT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine works on any formatted data files, applying a
*     threshold to one of the variables. The variable is specified by a
*     column number. File entries with variable value above and below
*     the threshold are written out to separate files.

*  Usage:
*     PISACUT INPUT COLUMN THRESH LOWER HIGHER

*  ADAM Parameters:
*     INPUT = FILENAME (Read)
*        Name of a file containing the data to be separated by applying
*        a threshold value to the variable in the specified column.
*     COLUMN = _INTEGER (Read)
*        The column which contains the data which is to be thresholded.
*        [1]
*     THRESH = _REAL (Read)
*        The threshold value for variable in the given column. Entries
*        with this variable value above the threshold will be entered
*        to file HIGHER. Entries with a value less than or equal to
*        this variable value will be entered into file LOWER. [0.0]
*     LOWER = FILENAME (Write)
*        Name of a file to contain the entries with selected variable
*        value less than equal to the threshold.
*     HIGHER = FILENAME (Write)
*        Name of a file to contain the entries with selected variable
*        value greater than the threshold.

*  Examples:
*     PISACUT INPUT=PISAPEAK.DAT COLUMN=2 THRESH=1.5 LOWER=STARS
*        HIGHER=GALS
*        This separates the results from a run of the PISAPEAK program,
*        whose results are stored in file PISAPEAK.DAT, into two
*        different files STARS and GALS. The variable selected is the
*        one found in column 2 (the object peakedness). Entries,
*        in file PISAPEAK, with the variable value in column 2 greater
*        than 1.5 are written into file GALS, those with selected
*        variable value less than equal to 1.5 are written to file
*        STARS.
*

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-MAR-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system status values
      INCLUDE 'CHR_ERR'          ! chr library status values
      INCLUDE 'FIO_PAR'          ! FIO parameters

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! length of string excluding trailing
                                 ! blanks

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME ! File name
      CHARACTER * ( 256 ) LINE   ! Buffer to read file entries
      INTEGER I                  ! Loop variable
      INTEGER ICOL               ! the column number of variable
      INTEGER IEND               ! variable
      INTEGER IFS1               !
      INTEGER IFS2               ! FIO file descriptors
      INTEGER IFS3               !
      INTEGER IHIGH              ! files
      INTEGER ILOW               ! Numbers of entries written to output
      INTEGER ISTART             ! First and last characters in
      INTEGER NCHAR              ! number of characters in one line
      LOGICAL OPNF1              !
      LOGICAL OPNF2              ! Set if files are open
      LOGICAL OPNF3              !
      REAL RVAL                  ! current value of the threshold variable
      REAL THRESH                ! the threshold value of variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the file containing the data.
      CALL PSA1_ASFIO( 'INPUT', 'READ', 'LIST', 0, IFS1, OPNF1,
     :                 STATUS )

*  Get the data column.
      CALL PAR_GET0I( 'COLUMN', ICOL, STATUS )

*  Get the variable threshold.
      CALL PAR_GET0R( 'THRESH', THRESH, STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 99

*  Open the files to contain the thresholded data.
      CALL PSA1_ASFIO( 'LOWER', 'WRITE', 'LIST', 0, IFS2, OPNF2,
     :                 STATUS )
*  Second file
      CALL PSA1_ASFIO( 'HIGHER', 'WRITE', 'LIST', 0, IFS3, OPNF3,
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Loop while reading in data from the file.
      ILOW = 0
      IHIGH = 0
 4    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( .TRUE. ) THEN
         LINE = ' '
         CALL FIO_READ( IFS1, LINE, NCHAR, STATUS )
         IF ( STATUS .EQ. FIO__EOF) THEN
            CALL ERR_ANNUL( STATUS )
            GO TO 6
         END IF

*  Loop until have the correct variable.
         IEND = 0
         DO 5 I = 1, ICOL
            ISTART = IEND + 1
            CALL CHR_FIWS( LINE, ISTART, STATUS )
            IEND = ISTART
            CALL CHR_FIWE( LINE, IEND, STATUS )
 5       CONTINUE

*  If we have a status then issue error report.
         IF( STATUS .EQ. CHR__ENDOFSENT .OR.
     :       STATUS .EQ. CHR__WRDNOTFND ) THEN
             CALL ERR_REP( 'BADCOL',
     :       'Error finding specified column', STATUS )
             GO TO 99
         END IF

*  Decode value.
         CALL CHR_CTOR( LINE( ISTART : IEND ), RVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'BADVAL',
     :      'Cannot read value in file', STATUS )
            GO TO 99
         END IF

*  Threshold and write to appropriate file.
         IF ( RVAL .GT. THRESH ) THEN
            CALL FIO_WRITE( IFS3, LINE( : NCHAR ), STATUS )
            IHIGH = IHIGH + 1
         ELSE
            CALL FIO_WRITE( IFS2, LINE( : NCHAR ), STATUS )
            ILOW = ILOW + 1
         END IF

*  Next line.
         GO TO 4
      END IF
6     CONTINUE

*  Write out an informational message on the number of entrie in each
*  output file.
      CALL FIO_FNAME( IFS2, FNAME, STATUS )
      CALL MSG_SETC( 'FNAME', FNAME )
      CALL MSG_SETI( 'LOWER_ENTRIES', ILOW )
      CALL MSG_OUT( 'MESSAGE_OUT1',
     : ' ^LOWER_ENTRIES entries written to file ^FNAME', STATUS )
      CALL FIO_FNAME( IFS3, FNAME, STATUS )
      CALL MSG_SETC( 'FNAME', FNAME )
      CALL MSG_SETI( 'HIGHER_ENTRIES', IHIGH )
      CALL MSG_OUT( 'MESSAGE_OUT2',
     : ' ^HIGHER_ENTRIES entries written to file ^FNAME', STATUS )

*  Error label - arrive directly here if exiting with status set
99    CONTINUE

*  Close opened files.
      IF( OPNF1 ) THEN
         CALL FIO_CLOSE( IFS1, STATUS )
         CALL PAR_CANCL( 'INPUT', STATUS )
      END IF
      IF( OPNF2 ) THEN
         CALL FIO_CLOSE( IFS2, STATUS )
         CALL PAR_CANCL( 'LOWER', STATUS )
      END IF
      IF( OPNF3 ) THEN
         CALL FIO_CLOSE( IFS3, STATUS )
         CALL PAR_CANCL( 'HIGHER', STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISACUT_ERR',
     :   'PISACUT: Error applying variable threshold cut.',
     :   STATUS )
      END IF

      END
* $Id$
