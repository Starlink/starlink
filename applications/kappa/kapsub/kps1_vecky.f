      SUBROUTINE KPS1_VECKY( PARKEY, VSCALE, AHSIZM, KDATA, UNITS, JUST,
     :                       STATUS )
*+
*  Name:
*     KPS1_VECKY

*  Purpose:
*     Draw a key for a vector map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_VECKY( PARKEY, VSCALE, AHSIZM, KDATA, UNITS, JUST, 
*                      STATUS )

*  Description:
*     The key consists of a text string describing the scale in terms
*     of centimetres per data unit, plus a graphical vector.  The
*     length of this vector is obtained from the environment, with a
*     dynamic default corresponding to a round data value close to the
*     supplied data value (KDATA).  If the corresponding vector
*     occupies more than 0.75 of the width of the zone, then a smaller
*     value is used.  A text string describing the data value to which
*     the graphical vector corresponds is drawn underneath the vector.
*     The key is drawn in the current SGS zone.

*  Arguments:
*     PARKEY = CHARACTER * ( * ) (Given)
*        The name of the parameter to use to get the length of the
*        required key vector (in data units).
*     VSCALE = REAL (Given)
*        The scale with which the vectors are plotted, in data units
*        per centimetre.
*     AHSIZM = REAL (Given)
*        The size of the arrowhead to be drawn at the end of the
*        vector, in metres.  If a value of zero is supplied, no arrow
*        head is drawn.
*     KDATA = REAL (Given)
*        The data value corresponding to the key vector which is to be
*        drawn.  A lower value may be selected by the routine.
*     UNITS = CHARACTER * ( * ) (Given)
*        A string describing the units of the vector magnitude data.
*        This should be allocated at least 27 characters to avoid
*        unnecessary truncation.
*     JUST = CHARACTER * ( * ) (Given)
*        This can be 'CENTRE', 'START' or 'END'.  'CENTRE' causes
*        vectors to be drawn centred on the corresponding pixel.
*        'START' causes vectors to be drawn starting at the
*        corresponding pixel.  'END' causes vectors to be drawn ending
*        at the corresponding pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1993 (DSB):
*        Original version.
*     1995 April 13 (MJC):
*        Corrected millimetre documentation error and typo's.  Made
*        minor stylistic changes.  Used modern-style variable
*        declarations.  
*     1995 April 19 (MJC):
*        Improved the format of the scale value.  Left-justify the
*        text for consistency with other keys in KAPPA.  Used more of
*        the key width for the text (0.7 to 0.9) so as to make the text
*        size larger.  Allowed a longer title if the values in the key
*        are short and made allowance for proportionally spaced text.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARKEY
      REAL VSCALE
      REAL AHSIZM
      REAL KDATA
      CHARACTER * ( * ) UNITS
      CHARACTER * ( * ) JUST

*  Status:
      INTEGER STATUS             ! Global status

*  Extrenal References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Constants:
      REAL AR                    ! A `normal' text aspect ratio
      PARAMETER ( AR = 0.66667 )

      REAL CIRAD                 ! Radius of circle marking vector posn.
      PARAMETER ( CIRAD = 0.015 )

      REAL DTOR                  ! Degress-to-radians conversion factor
      PARAMETER ( DTOR = 1.7453293E-2 )

      REAL TXTHGT                ! Nominal text height
      PARAMETER ( TXTHGT = 0.025 )

      REAL VECLEN                ! Vector len. as fraction of zone width
      PARAMETER ( VECLEN = 0.75 )

      REAL XWIDTH                ! Max. text length as fraction of zone 
      PARAMETER ( XWIDTH = 0.7 ) ! width

*  Local Variables:
      REAL AHSIZE                ! Arrowhead size in world coordinates
      REAL AR0                   ! Text aspect ratio on entry
      REAL BASE                  ! Power of ten just less than KEYDAT
      INTEGER CUNITS             ! Number of characters in units
      REAL HGT                   ! Height for key text
      REAL HT0                   ! Text height on entry
      REAL KEYDAT                ! Data value for key vector
      REAL KEYLEN                ! Vector length in centimetres
      INTEGER KEYNC              ! Significant length of KEYTXT
      CHARACTER * ( 80 ) KEYTXT  ! Text describing the key vector
      INTEGER MAXNC              ! Length of longest line of text
      INTEGER MXUNIT             ! Max. number of characters in units
      INTEGER NF                 ! Fount number
      INTEGER NPR                ! Text precision
      REAL RADIUS                ! Used radius of the circle
      REAL SP0                   ! Space between characters on entry
      CHARACTER * ( 2 ) TXJ0     ! Text justification on entry
      INTEGER VSCNC              ! Significant length of VSCTXT
      CHARACTER * ( 80 ) VSCTXT  ! Text describing scale
      REAL X1                    ! Lower x bound of key zone
      REAL X2                    ! Upper x bound of key zone
      REAL XL                    ! X co-ordinate of left of text
      REAL XM                    ! X extent of key zone, in metres
      REAL XU0                   ! X comp. of text up-vector on entry
      REAL Y1                    ! Lower y bound of key zone
      REAL Y2                    ! Upper y bound of key zone
      REAL YC                    ! Y coord. of centre of key object
      REAL YM                    ! Y extent of key zone, in metres
      REAL YU0                   ! Y comp. of text up-vector on entry
      INTEGER ZONE0              ! ID for SGS zone current on entry
      INTEGER ZONE2              ! ID for uniform SGS zone covering key

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if the vector scale is zero.
      IF ( VSCALE .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_VECKY_ZEROSCA', 'KPS1_VECKY: Zero vector '/
     :                 /'scale supplied (programming error).', STATUS )
         GO TO 999
      END IF

*  Save an identifier for the current zone.
      CALL SGS_ICURZ( ZONE0 )

*  Get the extent of the current zone.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Create a new zone covering the same area as the current zone which
*  has a uniform co-ordinate system (i.e. the same scale on each axis).
      CALL SGS_ZONE( X1, X2, Y1, Y2, ZONE2, STATUS )

*  Get the extent in world co-ordinates of the new zone.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Abort if an error has been reported, or the zone has zero size.
      IF ( STATUS .NE. SAI__OK .OR.
     :     XM .LE. 0.0 .OR. YM .LE. 0.0 ) GO TO 999

*  Get the current text attributes.
      CALL SGS_ITXA( NF, NPR, HT0, AR0, XU0, YU0, SP0, TXJ0 )

*  Ensure characters are drawn with a `normal' aspect ratio.
      CALL SGS_SARTX( AR )

*  Ensure characters are drawn horizontally, from left to right.
      CALL SGS_SUPTX( 0.0, 1.0 )

*  Ensure there is no space between characters.
      CALL SGS_SSPTX( 0.0 )

*  Text is position by specifying the world co-ordinates at which the
*  left-hand centre of the string is to be placed.
      CALL SGS_STXJ( 'CL' )

*  Format the vector scale value (data units per centimetre).  Try
*  using the CHR routine to give a pleasing format (i.e. no E numbers
*  for moderate values and so give greater precision).  However, if
*  this uses more than the maximum allowed 9 characters, reformat using
*  the G specifier, remove leading blanks and get its used length.
      CALL CHR_RTOC( VSCALE, VSCTXT, VSCNC )
      IF ( VSCNC .GT. 9 ) THEN
         VSCTXT( 1:VSCNC ) = ' '
         WRITE( VSCTXT, '(G9.2)' ) VSCALE
         CALL CHR_LDBLK( VSCTXT )
         VSCNC = CHR_LEN( VSCTXT )
      END IF

*  Increment the position of the last significant character in order to
*  produce a space after the VSCALE value.
      VSCNC = VSCNC + 1

*  Decide how much of the units string will appear in the scale
*  caption.  Get the length of the UNITS.  The allowed length in the
*  caption is 32 characters less the space and the value, and 3 for the
*  "/cm" in the first scale specification.
      IF ( UNITS .NE. ' ' ) THEN
         CUNITS = CHR_LEN( UNITS )
         MXUNIT = 29 - VSCNC

*  Append the data units to the scale string.  Truncate the string if
*  necessary, terminating with an ellipsis.  Otherwise just use the
*  supplied units string.  For a null input units, set a default of
*  "Data Units".
         IF ( CUNITS .GT. MXUNIT ) THEN
            CALL CHR_APPND( UNITS( 1:MXUNIT-3 ), VSCTXT, VSCNC )
            CALL CHR_APPND( '...', VSCTXT, VSCNC )
         ELSE
            CALL CHR_APPND( UNITS, VSCTXT, VSCNC )
         END IF
      ELSE
         CALL CHR_APPND( 'Data Units', VSCTXT, VSCNC )
      END IF

*  Now append "/cm"  (i.e. "per centimetre").
      CALL CHR_APPND( '/cm', VSCTXT, VSCNC )

*  Find the data value corresponding to a vector length equal to the
*  required fraction of the width of the key zone.  If the supplied data
*  value is smaller than this, use it.
      KEYDAT = ABS( VECLEN * 100.0 * XM * VSCALE )
      IF ( KDATA .GT. 0.0 .AND. KDATA .LT. KEYDAT ) KEYDAT = KDATA

*  This data value will not in general be a round number.  Find a
*  pleasing round data value which produces a similar vector length.
*  BASE is assigned the power of ten just less than the initial value of
*  KEYDAT.
      BASE = 10.0**INT( LOG10( KEYDAT ) )
      IF ( KEYDAT .LT. 1.0 ) BASE = BASE * 0.1
      KEYDAT = REAL( MAX( 1, INT( KEYDAT/BASE ) ) ) * BASE

*  Get a new value, limiting it to 95% of the width of the key zone.
*  The value just calculated is used as the dynamic default.
      CALL PAR_GDR0R( PARKEY, KEYDAT, 0.0, 95.0 * XM * VSCALE, .FALSE.,
     :                KEYDAT, STATUS )

*  Format the data value into a character string.
      CALL CHR_RTOC( KEYDAT, KEYTXT, KEYNC )

*  Increment the position of the last significant character in KEYTXT in
*  order to produce a space between the value and the units string.
      KEYNC = KEYNC + 1

*  Decide how much of the units string will appear in the key caption.
*  Get the length of the UNITS.  The allowed length in the captions is
*  32 characters less the space and the value.
      IF ( UNITS .NE. ' ' ) THEN
         MXUNIT = 32 - KEYNC

*  Append the data units to the key string.  Truncate the string if
*  necessary, terminating with an ellipsis.  Otherwise just use the
*  supplied units string.  For a null input units, set a default of
*  "Data Units".
         IF ( CUNITS .GT. MXUNIT ) THEN
            CALL CHR_APPND( UNITS( 1:MXUNIT-3 ), KEYTXT, KEYNC )
            CALL CHR_APPND( '...', KEYTXT, KEYNC )
         ELSE
            CALL CHR_APPND( UNITS, KEYTXT, KEYNC )
         END IF
      ELSE
         CALL CHR_APPND( 'Data Units', KEYTXT, KEYNC )
      END IF

*  Find the maximum number of characters per line in the key.
      MAXNC = MAX( 13, KEYNC, VSCNC )

*  A text height equal to a fixed fraction of the vertical size of the
*  zone is used, unless this would result in the text being wider than
*  XWIDTH of the zone width, in which case a smaller text height is
*  used.  In practice the text may extend beyond the width because of
*  the particular proportionally spaced characters in the caption.
*  Therefore it is wise not to make XWIDTH larger than 0.7.
      HGT = MIN( TXTHGT * ( Y2 - Y1 ),
     :           XWIDTH * ( X2 - X1 ) / ( AR * REAL( MAXNC ) ) )
      CALL SGS_SHTX( HGT )

*  Produce text describing the vector scale in words.  Left justify
*  to allow room for the text to spill over the XWIDTH fraction.
      XL = X1 + 0.01 * ( X2 - X1 )
      YC = Y2 - HGT * 0.6
      CALL SGS_TX( XL, YC, 'Vector scale:' )

      YC = YC - 2.0 * HGT
      CALL SGS_TX( XL, YC, VSCTXT( : VSCNC ) )

*  Find the length of the key vector, in the world co-ordinate system
*  of the key zone.
      KEYLEN = ( X2 - X1 ) * KEYDAT / ( VSCALE * XM * 100.0 )

*  Convert the arrowhead size from metres to key-zone world
*  co-ordinates.
      AHSIZE = ( X2 - X1 ) * AHSIZM / XM

*  Define the y position of the vector, and the radius of the circle
*  indiciating the justification.  Draw the vector justified to the
*  left, but allowing room for the circle when the justification is
*  left justified too.
      YC = YC - 5.0 * HGT
      RADIUS = MIN( 0.1 * KEYLEN, ( X2 - X1 ) * CIRAD )

      IF ( JUST .EQ. 'START' ) THEN
         CALL KPG1_VECT( XL + 0.5 * KEYLEN + RADIUS, YC, 'CENTRE',
     :                   KEYLEN, -90.0 * DTOR, AHSIZE, STATUS )
      ELSE
         CALL KPG1_VECT( XL + 0.5 * KEYLEN, YC, 'CENTRE', KEYLEN,
     :                   -90.0 * DTOR, AHSIZE, STATUS )
      END IF

*  Draw a circle to mark the vector reference position.
      IF ( JUST .EQ. 'CENTRE' ) THEN
         CALL SGS_CIRCL( XL + 0.5 * KEYLEN, YC, RADIUS )

      ELSE IF ( JUST .EQ. 'START' ) THEN
         CALL SGS_CIRCL( XL + RADIUS, YC, RADIUS )

      ELSE
         CALL SGS_CIRCL( XL + 1.0 * KEYLEN, YC, RADIUS )
      END IF

*  Plot the text below the vector.
      CALL SGS_TX( XL, YC - 2.0 * HGT, KEYTXT( : KEYNC ) )

*  Reinstate the original text attributes.
      CALL SGS_SARTX( AR0 )
      CALL SGS_SUPTX( XU0, YU0 )
      CALL SGS_SSPTX( SP0 )
      CALL SGS_STXJ( TXJ0 )
      CALL SGS_SHTX( HT0 )

*  Flush the graphics.
      CALL SGS_FLUSH
      
*  Re-select the original zone.
      CALL SGS_SELZ( ZONE0, STATUS )

*  Release the intermediate zone.
      CALL SGS_RELZ( ZONE2 )

*  Arrive here if an error has occurred.
 999  CONTINUE

      END
