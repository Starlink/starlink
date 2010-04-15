      SUBROUTINE PXYGT2( LI, BUF, NOBJ, X, Y, MAG, STATUS )
*+
*  Name:
*     PXYGT2

*  Purpose:
*     To read in a line from a formatted file, parsing it and
*     recovering the first integer and next three reals. If the first
*     number is not an integer it is assumed to be the first real and
*     nobj is returned as zero.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PXYGT2( LI, BUF, NOBJ, X, Y, MAG, STATUS )

*  Description:
*     This routine reads in a line into the character buffer BUF
*     from a formatted file described by LI. This line is then parsed
*     in an attempt to extract first four fields, which should be the
*     object number (integer ) and the x and y positions (reals), and
*     the magnitude; if the attached file uses the present PISA
*     standard. If the first word in the buffer is not an integer then
*     it is assumed to be the x position and the next word the y
*     position. Blank lines are skipped.

*  Arguments:
*     LI = INTEGER (Given)
*        The FIO file descriptor
*     BUF = CHARACTER*(*) (Given)
*        Buffer to hold the line read from the file
*     NOBJ = INTEGER (Returned)
*        The object number read from file, set to zero if not present
*     X = REAL (Returned)
*        The x coordinate
*     Y = REAL (Returned)
*        The y coordinate
*     MAG = REAL (Returned)
*        The object magnitude
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-SEP-1990 (PDRAPER):
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
      INTEGER LI
      CHARACTER*(*) BUF

*  Arguments Returned:
      REAL X
      REAL Y
      REAL MAG
      INTEGER NOBJ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXWRD              ! maximum words returned from file line
                                 ! buffer
      PARAMETER ( MXWRD = 4 )
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

*
*  read in the line from file
1     CONTINUE
      CALL FIO_READ( LI, BUF, NREAD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
*
*  read in line attempt to extract first three words
         CALL CHR_DCWRD( BUF, MXWRD, NRET, START, STOP, WORDS, LSTAT )
         IF ( NRET .EQ. 3 ) THEN
*
*  have only two words must be x, y and magnitude to make any sense
            CALL CHR_CTOR( WORDS( 1 ), X, STATUS )
            CALL CHR_CTOR( WORDS( 2 ), Y, STATUS )
            CALL CHR_CTOR( WORDS( 3 ), MAG, STATUS )
         ELSE IF ( NRET .GT. 3 ) THEN
*
*  probably have all four fields

            CALL CHR_CTOI( WORDS( 1 ), NOBJ, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
*
*  first word integer, decode the next three as reals
               NOBJ = 0
               CALL CHR_CTOR( WORDS( 2 ), X, STATUS )
               CALL CHR_CTOR( WORDS( 3 ), Y, STATUS )
               CALL CHR_CTOR( WORDS( 4 ), MAG, STATUS )
            ELSE
*
*  unknown first word type try to decode first three as reals
               CALL CHR_CTOR( WORDS( 1 ), X, STATUS )
               CALL CHR_CTOR( WORDS( 2 ), Y, STATUS )
               CALL CHR_CTOR( WORDS( 3 ), MAG, STATUS )
            END IF
         ELSE
*
*  bad return probably a blank line skip it
            GO TO 1
         END IF
      END IF

      END
* $Id$
