
      SUBROUTINE RDPISD( LI, BUF, INDEX, A1, A2, A3, A4, A5, A6, A7,
     :                   A8, STATUS )
*+
*  Name:
*     RDPIFD - READ PISA SIZE DATA

*  Purpose:
*     To read in a line of data from a PISASIZE type file, returning the
*     values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDPIFD( LI, BUF, INDEX, A1, A2, A3, A4, A5, A6, A7,
*                  A8, STATUS )

*  Description:
*     This routine reads in a line into the character buffer BUF
*     from a formatted file described by LI.
*     This line is then parsed into 9 separate words which returned as
*     the PISASIZE data values.

*  Arguments:
*     LI = INTEGER (Given)
*        The FIO file descriptor
*     BUF = CHARACTER*(*) (Given)
*        Buffer to hold the line read from the file
*     INDEX = INTEGER (Returned)
*        Object index number.
*     A1 - A8 = REAL (Returned)
*        The value of the number of pixels within the thresholds

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1991 (PDRAPER):
*        Original Version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system error codes

*  Arguments Given:
      INTEGER LI
      CHARACTER*(*) BUF

*  Arguments Returned:
      INTEGER INDEX
      REAL A1
      REAL A2
      REAL A3
      REAL A4
      REAL A5
      REAL A6
      REAL A7
      REAL A8

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXWRD              ! maximum words returned from file line
                                 ! buffer
      PARAMETER ( MXWRD = 9 )
      INTEGER WRDLEN             ! maximum length of words returned
      PARAMETER ( WRDLEN = 14 )

*  Local Variables:
      INTEGER NREAD              ! number of characters read from file
      INTEGER NRET               ! actual number of words in buf
      INTEGER START( MXWRD )     ! starting positions of words located
      INTEGER STOP( MXWRD )      ! end positions of words located
      CHARACTER*( WRDLEN ) WORDS( MXWRD ) ! the first mxwrds if they
                                 ! exist
      INTEGER LSTAT              ! local status return

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  read in the line from file
1     CONTINUE
      CALL FIO_READ( LI, BUF, NREAD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  read in line attempt to extract first three words
         CALL CHR_DCWRD( BUF, MXWRD, NRET, START, STOP, WORDS, LSTAT )
         IF ( NRET .EQ. MXWRD ) THEN

*  have all fields convert them into values

            CALL CHR_CTOI( WORDS( 1 ), INDEX, STATUS )
            CALL CHR_CTOR( WORDS( 2 ), A1, STATUS )
            CALL CHR_CTOR( WORDS( 3 ), A2, STATUS )
            CALL CHR_CTOR( WORDS( 4 ), A3, STATUS )
            CALL CHR_CTOR( WORDS( 5 ), A4, STATUS )
            CALL CHR_CTOR( WORDS( 6 ), A5, STATUS )
            CALL CHR_CTOR( WORDS( 7 ), A6, STATUS )
            CALL CHR_CTOR( WORDS( 8 ), A7, STATUS )
            CALL CHR_CTOR( WORDS( 9 ), A8, STATUS )
         ELSE IF( NRET .LT. MXWRD .AND. NRET .GT. 0 ) THEN

*  Bad number of words in line
            STATUS = SAI__ERROR
            GO TO 99
         ELSE

*  bad return probably a blank line skip it
            GO TO 1
         END IF
      END IF
 99   CONTINUE
      END
* $Id$
