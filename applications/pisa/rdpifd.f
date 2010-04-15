      SUBROUTINE RDPIFD( LI, BUF, INDEX, XPOS, YPOS, INTENS, NPIX,
     :                   PEAK, ELLIP, ANGLE, SXX, SYY, SXY, STATUS )
*+
*  Name:
*     RDPIFD - READ PISA FIND DATA

*  Purpose:
*     To read in a line of data from a PISAFIND type file, returning the
*     values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDPIFD( LI, BUF, INDEX, XPOS, YPOS, INTENS, NPIX,
*                  PEAK, ELLIP, ANGLE, SXX, SYY, SXY, STATUS )

*  Description:
*     This routine reads in a line into the character buffer BUF
*     from a formatted file described by LI.
*     This line is then parsed into 11 separate words which returned as
*     the PISAFIND data values.

*  Arguments:
*     LI = INTEGER (Given)
*        The FIO file descriptor
*     BUF = CHARACTER*(*) (Given)
*        Buffer to hold the line read from the file
*     INDEX = INTEGER (Returned)
*        Object index number.
*     XPOS = REAL (Returned)
*        The x position of object.
*     YPOS = REAL (Returned)
*        The y position of object.
*     INTENS = REAL (Returned)
*        The integrated intensity.
*     NPIX = INTEGER (Returned)
*        The number of pixels above threshold.
*     PEAK = REAL (Returned)
*        The peak intensity in a pixel.
*     ELLIP = REAL (Returned)
*        The ellipticity.
*     ANGLE = REAL (Returned)
*        The postion angle.
*     SXX = REAL (Returned)
*        The x axis intensity weighted moment.
*     SYY = REAL (Returned)
*        The y axis intensity weighted moment.
*     SXY = REAL (Returned)
*        The cross x and y axis intensity weighted moment.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JAN-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

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
      REAL XPOS
      REAL YPOS
      REAL INTENS
      INTEGER NPIX
      REAL PEAK
      REAL ELLIP
      REAL ANGLE
      REAL SXX
      REAL SYY
      REAL SXY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXWRD              ! maximum words returned from file line
                                 ! buffer
      PARAMETER ( MXWRD = 11 )
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
            CALL CHR_CTOR( WORDS( 2 ), XPOS, STATUS )
            CALL CHR_CTOR( WORDS( 3 ), YPOS, STATUS )
            CALL CHR_CTOR( WORDS( 4 ), INTENS, STATUS )
            CALL CHR_CTOI( WORDS( 5 ), NPIX, STATUS )
            CALL CHR_CTOR( WORDS( 6 ), PEAK, STATUS )
            CALL CHR_CTOR( WORDS( 7 ), ELLIP, STATUS )
            CALL CHR_CTOR( WORDS( 8 ), ANGLE, STATUS )
            CALL CHR_CTOR( WORDS( 9 ), SXX, STATUS )
            CALL CHR_CTOR( WORDS( 10 ), SYY, STATUS )
            CALL CHR_CTOR( WORDS( 11 ), SXY, STATUS )
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
