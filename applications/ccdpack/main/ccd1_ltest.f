      SUBROUTINE CCD1_LTEST( FD, BUF, BUFLEN, MINVAL, MAXVAL, NVAL,
     :                       STATUS )
*+
*  Name:
*     CCD1_LTEST

*  Purpose:
*     Returns the number of words in the first line of a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LTEST( FD, BUF, BUFLEN, MINVAL, MAXVAL, NVAL, STATUS )

*  Description:
*     This routine reads in the first line from a formatted file
*     attached to the FIO FD file descriptor. It breaks this line up
*     into words (which may be comma and/or space separated). The number
*     of words which are located is returned if the number falls within
*     the range MINVAL to MAXVAL. If the number of words does not fall
*     in this range STATUS is set and an error is reported. If either of
*     the bounds is given as 0 then it does not apply. The file is
*     rewound to the beginning before the routine returns.

*  Arguments:
*     FD = INTEGER (Given)
*        FIO system file descriptor.
*     BUF = CHARACTER * ( BUFLEN ) (Given and Returned)
*        Buffer for reading in line from file.
*     BUFLEN = INTEGER (Given)
*        Length of BUF.
*     MINVAL = INTEGER (Given)
*        The minimum number of words which is acceptable. If this
*        value is given as zero no bound applies.
*     MAXVAL = INTEGER (Given)
*        The maximum number of words which is acceptable. If this
*        value is given as zero no bound applies.
*     NVAL = INTEGER (Returned)
*        The number of words located in line.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-SEP-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FD
      INTEGER BUFLEN
      INTEGER MAXVAL
      INTEGER MINVAL

*  Arguments Given and Returned:
      CHARACTER * ( * ) BUF

*  Arguments Returned:
      INTEGER NVAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) FNAME   ! File name used in error messages
      INTEGER LINNUM             ! Current line number in file == 1
      INTEGER NCHAR              ! Number of characters read from file
      LOGICAL EOF                ! True if EOF reached (empty file)
      LOGICAL HAVONE             ! True if file contains at least one
                                 ! value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set line number for error messages.
      LINNUM = 1

*  Read in the line.
      CALL CCD1_RDLIN( FD, BUFLEN, BUF, NCHAR, LINNUM, EOF, STATUS )

*  Decode line to find out how many values it has.
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. EOF ) THEN
         CALL CCD1_DECL2( BUF, LINNUM, HAVONE, NVAL, STATUS )

*  If at least one value is present then increment the number of values
*  to add this back. It is usually assumed to be an index and is hence
*  removed from the usual number of values count.
         IF ( HAVONE ) NVAL = NVAL + 1

*  Check that the number of words is  within the bounds.
         IF ( MAXVAL .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            IF ( NVAL .GT. MAXVAL ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'MAXVAL', MAXVAL )
               CALL ERR_REP( 'CCD1_LTEST1',
     :         '  The number of values exceeds the maximum'//
     :         ' permitted (^MAXVAL)',
     :         STATUS )
            END IF
         END IF
         IF ( MINVAL .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            IF ( NVAL .LT. MINVAL ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'MINVAL', MINVAL )
               CALL ERR_REP( 'CCD1_LTEST2',
     :         '  The number of values is less than the minimum'//
     :         ' permitted (^MINVAL)', STATUS )
            END IF         
         END IF

*  If status is BAD issue filename with error message
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'FILENAME', FNAME )
            CALL ERR_REP( 'FILEREADERR',
     :      '  Error reading file ^FILENAME', STATUS )
            GO TO 99
         END IF
      END IF

*  Rewind the file and exit.
      CALL FIO_RWIND( FD, STATUS )

 99   CONTINUE
      END
* $Id$
