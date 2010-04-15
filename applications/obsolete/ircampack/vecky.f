      SUBROUTINE VECKY( PARKEY, VSCALE, AHSIZM, KDATA, UNITS, JUST,
     :                  STATUS )
*+
*  Name:
*     VECKY

*  Purpose:
*     Draw a key for a vector map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL VECKY( PARKEY, VSCALE, AHSIZM, KDATA, UNITS, JUST, STATUS )

*  Description:
*     The key consists of a text string describing the scale in terms
*     of millimetres per data unit, plus a graphical vector. The length
*     of this vector is obtained from the environment, with a dynamic
*     default corresponding to a nice round data value close to the
*     supplied data value (KDATA). If the corresponding vector occupies
*     more than 0.75 of the width of the zone, then a smaller value is
*     used.  A text string describing the data value to which the
*     graphical vector corresponds is drawn underneath the vector.  The
*     key is drawn in the current SGS zone.

*  Arguments:
*     PARKEY = CHARACTER * ( * ) (Given)
*        The name of the parameter to use to get the length of the
*        required key vector (in data units).
*     VSCALE = REAL (Given)
*        The scale with which the vectors are plotted, in data units
*        per centimetre.
*     AHSIZM = REAL (Given)
*        The size of the arrow head to be drawn at the end of the
*        vector, in metres. If a value of zero is supplied, no arrow
*        head is drawn.
*     KDATA = REAL (Given)
*        The data value correspnding to the key vector which is to be
*        drawn. A lower value may be used.
*     UNITS = CHARACTER * ( * ) (Given)
*        A string describing the units of the vector magnitude data.
*     JUST = CHARACTER * ( * ) (Given)
*        CENTRE, START or END. CENTER causes vectors to be drawn
*        centred on the corresponding pixel. START causes vectors to be
*        drawn starting at the corresponding pixel. END causes vectors
*        to be drawn ending at the corresponding pixel.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1993 (DSB):
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
      CHARACTER PARKEY*(*)
      REAL VSCALE
      REAL AHSIZM
      REAL KDATA
      CHARACTER UNITS*(*)
      CHARACTER JUST*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Extrenal References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Constants:
      REAL AR                    ! A "normal" text aspect ratio
      PARAMETER ( AR = 0.66667 )

      REAL CIRAD                 ! Radius of circle marking vector posn.
      PARAMETER ( CIRAD = 0.015 )

      REAL DTOR                  ! Degress to radians conversion factor
      PARAMETER ( DTOR = 1.7453293E-2 )

      REAL TXTHGT                ! Nominal text height
      PARAMETER ( TXTHGT = 0.025 )

      REAL VECLEN                ! Vector len. as fraction of zone width
      PARAMETER ( VECLEN = 0.75 )

      REAL YFRAC                 ! Y position of the vector as a
                                 ! fraction of the zone height
      PARAMETER ( YFRAC = 0.91 )

*  Local Variables:
      CHARACTER
     :          KEYTXT*80,       ! Text describing the key vector
     :          TXJ0*2,          ! Text justification on entry
     :          VSCTXT*80        ! Text describing scale

      INTEGER
     :          KEYNC,           ! Significant length of KEYTXT
     :          MAXNC,           ! Length of longest line of text
     :          NF,              ! Font number
     :          NPR,             ! Text precision
     :          VSCNC,           ! Significant length of VSCTXT
     :          ZONE0,           ! ID for SGS zone current on entry
     :          ZONE2            ! ID for uniform SGS zone covering key

      REAL
     :          AHSIZE,          ! Arrow head size in world coordinates
     :          AR0,             ! Text aspect ratio on entry
     :          BASE,            ! Power of ten just less than KEYDAT
     :          HGT,             ! Height for key text
     :          HT0,             ! Text height on entry
     :          KEYDAT,          ! Data value for key vector
     :          KEYLEN,          ! Vector length in millimetres
     :          SP0,             ! Space between characters on entry
     :          X1,              ! Lower X bound of key zone
     :          X2,              ! Upper X bound of key zone
     :          XC,              ! X coord. of centre of object
     :          XM,              ! X extent of key zone, in metres
     :          XU0,             ! X comp. of text up-vector on entry
     :          Y1,              ! Lower Y bound of key zone
     :          Y2               ! Upper Y bound of key zone

      REAL
     :          YC,              ! Y coord. of centre of key object
     :          YM,              ! Y extent of key zone, in metres
     :          YU0              ! Y comp. of text up-vector on entry

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if the vector scale is zero.
      IF( VSCALE .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'VECKY_ZEROSCA', 'VECKY: Zero vector scale '//
     :                 'supplied (programming error).', STATUS )
         GO TO 999
      END IF

*  Save an identifier for the current zone.
      CALL SGS_ICURZ( ZONE0 )

*  Get the extent of the current zone.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Create a new zone covering the same area as the current zone which
*  has a uniform coordinate system (i.e. the same scale on each axis).
      CALL SGS_ZONE( X1, X2, Y1, Y2, ZONE2, STATUS )

*  Get the extent in world coordinates of the new zone.
      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*  Abort if an error has been reported, or the zone has zero size.
      IF ( STATUS .NE. SAI__OK .OR.
     :     XM .LE. 0.0 .OR. YM .LE. 0.0 ) GO TO 999

*  Get the current text attributes.
      CALL SGS_ITXA( NF, NPR, HT0, AR0, XU0, YU0, SP0, TXJ0 )

*  Ensure characters are drawn with a "normal" aspect ratio.
      CALL SGS_SARTX( AR )

*  Ensure characters are drawn horizontally, from left to right.
      CALL SGS_SUPTX( 0.0, 1.0 )

*  Ensure there is no space between characters.
      CALL SGS_SSPTX( 0.0 )

*  Text is position by specifying the world coordinates at which the
*  centre of the string is to be placed.
      CALL SGS_STXJ( 'CC' )

*  Format the vector scale value (data units per centimetre), remove
*  leading blanks and get its used length.
      WRITE( VSCTXT, '(G9.2)' ) VSCALE
      CALL CHR_LDBLK( VSCTXT )
      VSCNC = CHR_LEN( VSCTXT )

*  Increment the position of the last significant character in order to
*  produce a space after the VSCALE value.
      VSCNC = VSCNC + 1

*  Append the data units to the string (use "Data units" if no
*  data units are suplied).
      IF( UNITS .NE. ' ' ) THEN
         CALL CHR_APPND( UNITS, VSCTXT, VSCNC )
      ELSE
         CALL CHR_APPND( 'Data Units', VSCTXT, VSCNC )
      END IF

*  Now append "/cm"  (i.e. "per centimetre").
      CALL CHR_APPND( '/cm', VSCTXT, VSCNC )

*  Find the data value corresponding to a vector length equal to the
*  required fraction of the width of the key zone. If the supplied data
*  value is smaller than this , use it.
      KEYDAT = ABS( VECLEN*100.0*XM*VSCALE )
      IF( KDATA .GT. 0.0 .AND. KDATA .LT. KEYDAT ) KEYDAT = KDATA

*  This data value will not in general be a nice number.  Find a nice
*  round data value which produces a similar vector length.  BASE is
*  assigned the power of ten just less than the inital value of KEYDAT.
      BASE = 10.0**INT( LOG10( KEYDAT ) )
      IF( KEYDAT .LT. 1.0 ) BASE = BASE*0.1
      KEYDAT = REAL( MAX( 1, INT( KEYDAT/BASE ) ) )*BASE

*  Get a new value, limiting it to 95% of the width of the key zone. THE
*  Value just calculated is used as the dynamic default.
      CALL PAR_GDR0R( PARKEY, KEYDAT, 0.0, 95.0*XM*VSCALE, .FALSE.,
     :                KEYDAT, STATUS )

*  Format the data value into a character string.
      CALL CHR_RTOC( KEYDAT, KEYTXT, KEYNC )

*  Increment the position of the last significant character in KEYTXT in
*  order to produce a space between the value and the units string.
      KEYNC = KEYNC + 1

*  Append the data units string. Use "Data Units" if no units were
*  supplied.
      IF( UNITS .NE. ' ' ) THEN
         CALL CHR_APPND( UNITS, KEYTXT, KEYNC )
      ELSE
         CALL CHR_APPND( 'Data Units', KEYTXT, KEYNC )
      END IF

*  Find the maximum number of character per line in the key.
      MAXNC = MAX( 13, MAX( KEYNC, VSCNC ) )

*  A text height equal to a fixed fraction of the vertical size of the
*  zone is used, unless this would result in the text being wider than
*  0.7 of the zone width, in which case a smaller text height is used.
      HGT = MIN( TXTHGT*( Y2 - Y1 ),
     :           0.7*( X2 - X1 )/( AR*REAL( MAXNC ) ) )
      CALL SGS_SHTX( HGT )

*  Produce text describing the vector scale in words.
      XC = 0.5*( X1 + X2 )
      YC = Y2 - HGT*0.6
      CALL SGS_TX( XC, YC, 'Vector scale:' )

      YC = YC - 2.0*HGT
      CALL SGS_TX( XC, YC, VSCTXT( : VSCNC ) )

*  Find the length of the key vector, in the world coordinate system
*  of the key zone.
      KEYLEN = ( X2 - X1 )*KEYDAT/( VSCALE*XM*100.0 )

*  Convert the arrow head size from metres to key zone world
*  coordinates.
      AHSIZE = ( X2 - X1 )*AHSIZM/XM

*  Draw the vector.
      YC = YC - 5.0*HGT
      CALL VECT( XC, YC, 'CENTRE', KEYLEN, -90.0*DTOR, AHSIZE, STATUS )

*  Draw a circle to mark the vector reference position.
      IF( JUST .EQ. 'CENTRE' ) THEN
         CALL SGS_CIRCL( XC, YC, MIN( 0.1*KEYLEN, ( X2 - X1 )*CIRAD ) )

      ELSE IF( JUST .EQ. 'START' ) THEN
         CALL SGS_CIRCL( XC - 0.5*KEYLEN, YC,
     :                   MIN( 0.1*KEYLEN, ( X2 - X1 )*CIRAD ) )

      ELSE
         CALL SGS_CIRCL( XC + 0.5*KEYLEN, YC,
     :                   MIN( 0.1*KEYLEN, ( X2 - X1 )*CIRAD ) )
      END IF

*  Plot the text below the vector.
      CALL SGS_TX( XC, YC - 2.0*HGT, KEYTXT( : KEYNC ) )

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
