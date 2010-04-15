      SUBROUTINE SPOSB3( GOTSKY, INVER, SCS, IDA, LOGING, FD, TITLE,
     :                   A, B, X, Y, STATUS )
*+
*  Name:
*     SPOSB3

*  Purpose:
*     Display and log a position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPOSB3( GOTSKY, INVER, SCS, IDA, LOGING, FD, TITLE, A, B, X,
*                  Y, STATUS )

*  Description:
*     The supplied coordinates are transformed using the supplied
*     astrometry identifier, and then displayed on the terminal. They
*     are also logged to a log file if LOGING is true. On exit, A,B,X
*     and Y are all valid coordinates refering to the same position.

*  Arguments:
*     GOTSKY = LOGICAL (Given)
*        True if the supplied values of A and B hold the sky
*        coordinates of the position to be displayed.  False if the
*        supplied values of X and Y hold the image coordinates of the
*        position to be displayed.
*     INVER = LOGICAL (Given)
*        True if the user is interested primarily in image coordinates,
*        and false if he is interested primarily in sky coordinates.
*        This effects the order in which coordinates are displayed.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system in use.
*     IDA = INTEGER (Given)
*        The IRA astrometry identifier.
*     LOGING = LOGICAL (Given)
*        True if the position is to be loged to the file specified
*        by FD.
*     FD = INTEGER (Given)
*        An FIO identifier for the log file. Ignored if LOGING is false.
*     TITLE = LOGICAL (Given and Returned)
*        If true then a title is displayed for each column. TITLE is
*        always returned false.
*     A = DOUBLE PRECISION (Given and Returned)
*        The longitude of the position, either given or returned
*        depending on GOTSKY.
*     B = DOUBLE PRECISION (Given and Returned)
*        The latitude of the position, either given or returned
*        depending on GOTSKY.
*     X = REAL (Given and Returned)
*        The X image coordinate of the position, either given or
*        returned depending on GOTSKY.
*     Y = REAL (Given and Returned)
*        The Y image coordinate of the position, either given or
*        returned depending on GOTSKY.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      LOGICAL GOTSKY
      LOGICAL INVER
      CHARACTER SCS*(*)
      INTEGER IDA
      LOGICAL LOGING
      INTEGER FD

*  Arguments Given and Returned:
      LOGICAL TITLE
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      REAL X
      REAL Y

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Constants:
      INTEGER SZFIC              ! Field width for a formatted image
      PARAMETER ( SZFIC = 7 )    ! coordinate.

      INTEGER SZFSC              ! Field width for a formatted sky
      PARAMETER ( SZFSC = 15 )   ! coordinate (style 2).

      CHARACTER IFORM*6          ! Format specifier for formatting
      PARAMETER ( IFORM = '(F7.1)' )! image coordinates.

*  Local Variables:
      CHARACTER AABB*(IRA__SZSCA)! Abbreviation of sky longitude
      CHARACTER ATEXT*(IRA__SZFSC)! Formatted longitude.
      CHARACTER BABB*(IRA__SZSCA)! Abbreviation of sky latitude
      CHARACTER BTEXT*(IRA__SZFSC)! Formatted latitude.
      CHARACTER BUF*80           ! Buffer for log file output.
      CHARACTER DESCR*(IRA__SZSCD)! Description of longitude or latitude
      CHARACTER XTEXT*(SZFIC)    ! Formatted X value.
      CHARACTER YTEXT*(SZFIC)    ! Formatted Y value.

      DOUBLE PRECISION XX        ! X image coordinate.
      DOUBLE PRECISION YY        ! Y image coordinate.

      INTEGER BLEN               ! Used length of BUFFER.
      INTEGER LAA                ! Used length of AABB
      INTEGER LBA                ! Used length of BABB
      INTEGER LD                 ! Used length of DESCR
      INTEGER TLEN               ! Used length of a text string.

      LOGICAL OK                 ! True if coordinates can be printed.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise a flag to indicate that the coordinates cannot be printed.
      OK = .FALSE.

*  First deal with cases where the supplied positions are specified by
*  sky coordinates.
      IF( GOTSKY ) THEN

*  Initialise the returned image coordnates.
         X = VAL__BADR
         Y = VAL__BADR

*  Create formatted versions of the supplied sky coordinates.
         CALL IRA_DTOC( A, B, SCS, 0, ATEXT, BTEXT, STATUS )

*  Transform them to image coordinates.
         CALL IRA_TRANS( 1, A, B, .FALSE., SCS, IDA, XX, YY, STATUS )

*  Now check for bad image coordinates. This may happen if the supplied
*  sky coordinates refer to a position on the wrong side of the sky,
*  for instance.
         IF( XX .EQ. VAL__BADD .OR. YY .EQ. VAL__BADD ) THEN
            CALL MSG_SETC( 'A', ATEXT )
            CALL MSG_SETC( 'B', BTEXT )
            CALL MSG_OUTIF( MSG__NORM, 'SPOSB3_MSG1',
     :           '    Supplied position (^A, ^B) cannot be transformed',
     :                      STATUS )

*  Now check for image coordinates greater than 9999.9 These cannot be
*  fitted into the used field width.
         ELSE IF( ABS( XX ) .GT. 9999.9 ) THEN
            CALL MSG_SETC( 'A', ATEXT )
            CALL MSG_SETC( 'B', BTEXT )
            CALL MSG_OUTIF( MSG__NORM, 'SPOSB3_MSG2',
     :    '    X coordinate of supplied position (^A, ^B) is too large',
     :                      STATUS )

         ELSE IF( ABS( YY ) .GT. 9999.9 ) THEN
            CALL MSG_SETC( 'A', ATEXT )
            CALL MSG_SETC( 'B', BTEXT )
            CALL MSG_OUTIF( MSG__NORM, 'SPOSB3_MSG3',
     :    '    Y coordinate of supplied position (^A, ^B) is too large',
     :                      STATUS )

*  Otherwise, assign the image coordinates to a pair of message tokens
*  (displaying 1 decimal place) and indicate that the image coordinate
*  can be printed.
         ELSE
            X = REAL( XX )
            Y = REAL( YY )
            WRITE( XTEXT, IFORM ), X
            WRITE( YTEXT, IFORM ), Y
            OK = .TRUE.
         END IF

*  Now deal with cases where the supplied coordinates are image
*  coordinates.
      ELSE

*  Initialise the returned sky coordinates.
         A = VAL__BADD
         B = VAL__BADD

*  Assign the image coordinates to a pair of message tokens.
         CALL MSG_SETR( 'YY', Y )
         CALL MSG_SETR( 'XX', X )

*  Check that both are less than 9999.9
         IF( ABS( X ) .GT. 9999.9 ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'SPOSB3_MSG4',
     :       '    X coordinate of supplied position is too large (^XX)',
     :                      STATUS )

         ELSE IF( ABS( Y ) .GT. 9999.9 ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'SPOSB3_MSG5',
     :       '    Y coordinate of supplied position is too large (^YY)',
     :                      STATUS )

*  Otherwise, round the supplied image coordinates to one decimal place,
*  so that the displayed sky coordinates correspond to the displayed
*  image coordinates (which are displayed with one decimal place).
         ELSE
            XX = DBLE( NINT( X*10.0) )*0.1D0
            YY = DBLE( NINT( Y*10.0) )*0.1D0

*  Now transform them.
            CALL IRA_TRANS( 1, XX, YY, .TRUE., SCS, IDA, A, B, STATUS )

*  Now check for bad sky coordinates.
            IF( A .EQ. VAL__BADD .OR. A .EQ. VAL__BADD ) THEN
               CALL MSG_OUTIF( MSG__NORM, 'SPOSB3_MSG6',
     :          '    Image coordinates (^XX,^YY) cannot be transformed',
     :                         STATUS )

*  If both are OK, then indicate that the coordinates may be printed,
*  format the sky coordinates, and set up message tokens for the
*  coordinate values.
            ELSE
               OK = .TRUE.

               CALL IRA_DTOC( A, B, SCS, 0, ATEXT, BTEXT, STATUS )
               WRITE( XTEXT, IFORM ), REAL( XX )
               WRITE( YTEXT, IFORM ), REAL( YY )

            END IF

         END IF

      END IF

*  If the coordinates can be displayed...
      IF( OK ) THEN

*  Display a title if requested.
         IF( TITLE ) THEN

*  Get abbreviations for the the names of the longitude and latitude
*  in the sky coordinate system.
            CALL IRA_SCNAM( SCS, 1, DESCR, LD, AABB, LAA, STATUS )
            CALL IRA_SCNAM( SCS, 2, DESCR, LD, BABB, LBA, STATUS )

*  Initialise the output buffer, to overwrite any previous contents.
            BUF = ' '

*  Create the title string in the buffer.
            IF( .NOT. INVER ) THEN
               BUF( 6: ) = AABB( : MIN( LAA, SZFSC) )
               BUF( 9 + SZFSC: ) = BABB( : MIN( LBA, SZFSC) )
               BUF( 12 + 2*SZFSC + SZFIC: ) = 'X'
               BUF( 14 + 2*(SZFSC + SZFIC): ) = 'Y'

               BLEN = 15 + 2*(SZFSC + SZFIC)

            ELSE
               BUF( SZFIC: ) = 'X'
               BUF( 3 + 2*SZFIC: ) = 'Y'
               BUF( 18 + 2*SZFIC: ) = AABB( :MIN( LAA, SZFSC) )
               BUF( 20 + 2*SZFIC + SZFSC: ) = BABB( :MIN( LBA, SZFSC) )

               BLEN = 20 + 2*SZFIC + SZFSC + MIN( LBA, SZFSC )

            END IF

*  Write it to the screen.
            CALL MSG_OUTIF( MSG__NORM, 'SPOSB3_MSG7', BUF, STATUS )
            CALL MSG_OUTIF( MSG__NORM, 'SPOSB3_MSG8', ' ', STATUS )

*  Write it to the log file if required, placing a comment character (#)
*  in the first column.
            IF( LOGING ) THEN
               BUF( 1 : 1 ) = '#'
               CALL FIO_WRITE( FD, BUF( : MIN( 80, BLEN ) ),  STATUS )
               CALL FIO_WRITE( FD, ' ',  STATUS )
            END IF

*  Indicate that a title has been produced.
            TITLE = .FALSE.

         END IF

*  Initialise the output buffer, to overwrite any previous contents.
         BUF = ' '

*  Construct a buffer containing the text. The transformed coordinates
*  are put first, and the input coordinates are appended as an end of
*  line comment, so that the log file conforms to the format required
*  for input files to SKYPOS.
         IF( .NOT. INVER ) THEN

            TLEN = CHR_LEN( ATEXT )
            ATEXT( TLEN + 1 : TLEN + 1 ) = ','

            BUF( 3: ) = ATEXT
            BUF( 6 + SZFSC: ) = BTEXT
            BUF( 9 + 2*SZFSC: ) = '#  =  '
            BUF( 15 + 2*SZFSC: ) = XTEXT
            BUF( 15 + 2*SZFSC + SZFIC: ) = '  '
            BUF( 17 + 2*SZFSC + SZFIC: ) = YTEXT

            BLEN = 17 + 2*( SZFSC + SZFIC )

         ELSE
            TLEN = CHR_LEN( XTEXT )
            XTEXT( TLEN + 1 : TLEN + 1 ) = ','

            BUF( 3: ) = XTEXT
            BUF( 6 + SZFIC: ) = YTEXT
            BUF( 9 + 2*SZFIC: ) = '#  =  '
            BUF( 15 + 2*SZFIC: ) = ATEXT
            BUF( 15 + 2*SZFIC + SZFSC: ) = '  '
            BUF( 17 + 2*SZFIC + SZFSC: ) = BTEXT

            BLEN = 17 + 2*( SZFSC + SZFIC )

         END IF

*  Display the buffer on the screen.
         CALL MSG_OUTIF( MSG__NORM, 'SPOSB3_MSG9', BUF, STATUS )

*  If required write the buffer to the log file.
         IF( LOGING ) CALL FIO_WRITE( FD, BUF( : MIN( 80, BLEN ) ),
     :                                STATUS )

      END IF

      END
