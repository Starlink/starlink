      SUBROUTINE RDPIPD( LI, BUF, INDEX, PRATIO, IBYP, ELL, SXY,
     :                   STATUS )
*+
*  Name:
*     RDPIPD - READ PISA PEAK DATA

*  Purpose:
*     To read in one line of data from an PISAPEAK results file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDPIPD( LI, BUF, INDEX, PRATIO, IBYP, ELL, SXY,
*                  STATUS )

*  Description:
*     The routine reads in one line of the PISAPEAK data in FIO file
*     LI, returning the values. The lines are parsed using word
*     extraction routines not via fortran formatting.

*  Arguments:
*     LI = INTEGER (Given)
*        The FIO file descriptor
*     BUF = CHARACTER * ( * ) (Given and Returned)
*        Buffer to hold one line worth of information from the input
*        file.
*     INDEX = INTEGER (Returned)
*        Array of the first column values (indices) in the file LI.
*     PRATIO = REAL (Returned)
*        Array of the second column values in the file the LI.
*     IBYP = REAL (Returned)
*        Array of the third column values in the file the LI.
*     ELL = REAL (Returned)
*        Array of the fourth column values in the file the LI.
*     SXY = REAL (Returned)
*        Array of the fifth column values in the file the LI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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

*  Arguments Given and Returned:
      CHARACTER BUF * ( * )

*  Arguments Returned:
      INTEGER INDEX
      REAL PRATIO
      REAL IBYP
      REAL ELL
      REAL SXY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXWRD              ! maximum words returned from file line
                                 ! buffer
      PARAMETER ( MXWRD = 5 )
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

*  Read in the line from file
1     CONTINUE
      CALL FIO_READ( LI, BUF, NREAD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  read in line.
         CALL CHR_DCWRD( BUF, MXWRD, NRET, START, STOP, WORDS, LSTAT )
         IF ( NRET .EQ. MXWRD ) THEN

*  have all fields convert them into values

            CALL CHR_CTOI( WORDS( 1 ), INDEX, STATUS )
            CALL CHR_CTOR( WORDS( 2 ), PRATIO, STATUS )
            CALL CHR_CTOR( WORDS( 3 ), IBYP, STATUS )
            CALL CHR_CTOR( WORDS( 4 ), ELL, STATUS )
            CALL CHR_CTOR( WORDS( 5 ), SXY, STATUS )
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
