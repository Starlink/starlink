      SUBROUTINE POL1_VECKY( PARKEY, IPLOT, VSCALE, AHSIZM, KEYOFF,
     :                       KDATA, UNITS, JUST, HGTFAC, CLRKEY,
     :                       STATUS )
*+
*  Name:
*     POL1_VECKY

*  Purpose:
*     Draw a key for a vector map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_VECKY( PARKEY, IPLOT, VSCALE, AHSIZM, KEYOFF, KDATA, UNITS,
*                      JUST, HGTFAC, CLRKEY, STATUS )

*  Description:
*     The key consists of a text string describing the scale in terms
*     of centimetres per data unit, plus a graphical vector.  The
*     length of this vector is obtained from the environment, with a
*     dynamic default corresponding to a round data value close to the
*     supplied data value (KDATA).  If the corresponding vector
*     occupies more than 0.75 of the width of the picture, then a smaller
*     value is used.  A text string describing the data value to which
*     the graphical vector corresponds is drawn underneath the vector.
*     The key is drawn in the current PGPLOT viewport, which should
*     correspond to the current AGI picture, and which should have equal
*     scales on both axes.

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
*     KEYOFF = REAL (Given)
*        The fractional offset to the top of the key, where 0 and 1 are the
*        bottom and top of the key picture.
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
*     HGTFAC = REAL (Given)
*        The PGPLOT text height scaling factor corresponding to an keystyle
*        Size attribute value of 1.0.
*     CLRKEY = LOGICAL (Given)
*        If .TRUE., then the key is being drawn on top of the vector map.
*        This causes the background behind the key to be cleared before
*        drawing the key, and the key vector is drawn in the centre of
*        the key rather than at the left edge.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

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
*     5-MAR-1998 (DSB):
*        Modified for use with PGPLOT.
*     11-AUG-2000 (DSB):
*        Modified to allow suppression or replacement of key title text
*        using the Title attribute of the supplied Plot.
*     6-MAR-2006 (DSB):
*        Clear the background before drawing the key (useful if the key
*        is drawn over the vector map). Added argument CLRKEY.
*     13-APR-2007 (DSB):
*        Allow the user to select not to clear the key background by
*        setting the text backrgound colour to -1.
*     {enter_further_changes_here}


*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      CHARACTER * ( * ) PARKEY
      INTEGER IPLOT
      REAL VSCALE
      REAL AHSIZM
      REAL KDATA
      REAL KEYOFF
      CHARACTER * ( * ) UNITS
      CHARACTER * ( * ) JUST
      REAL HGTFAC
      LOGICAL CLRKEY

*  Status:
      INTEGER STATUS             ! Global status

*  Extrenal References:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Constants:
      REAL CIRAD                 ! Radius of circle marking vector posn.
      PARAMETER ( CIRAD = 0.015 )

      REAL DTOR                  ! Degress-to-radians conversion factor
      PARAMETER ( DTOR = 1.7453293E-2 )

      REAL VECMAX                ! Max. vector len. as fraction of picture width
      PARAMETER ( VECMAX = 0.8 )

      REAL VECMIN                ! Min. vector len. as fraction of picture width
      PARAMETER ( VECMIN = 0.4 )

*  Local Variables:
      CHARACTER KEYTXT*80        ! Text describing the key vector
      CHARACTER VSCTXT*80        ! Title text
      DOUBLE PRECISION ATTS( 20 )! Saved graphics attribute values
      INTEGER CI                 ! Original colour index
      INTEGER CI1                ! Lowest available colour index
      INTEGER CI2                ! Highest available colour index
      INTEGER CUNITS             ! Number of characters in units
      INTEGER FS                 ! Original fill style
      INTEGER FS0                ! Fill-area Style attribute on entry
      INTEGER KEYNC              ! Significant length of KEYTXT
      INTEGER MAXNC              ! Length of longest line of text
      INTEGER MXUNIT             ! Max. number of characters in units
      INTEGER TBG                ! Text background colour
      INTEGER VSCNC              ! Significant length of VSCTXT
      LOGICAL DRTOP              ! Draw the "Vector scale:" string?
      LOGICAL DRTTL              ! Draw any title string?
      REAL AHSIZE                ! Arrowhead size in world coordinates
      REAL BASE                  ! Power of ten just less than KEYDAT
      REAL GAP                   ! Vertical spacing factor
      REAL HGT                   ! Character height in world co-ordinates
      REAL KEYDAT                ! Data value for key vector
      REAL KEYLEN                ! Vector length in centimetres
      REAL RADIUS                ! Used radius of the circle
      REAL TY                    ! Y value at top of key
      REAL X1                    ! Lower x bound of key picture
      REAL X2                    ! Upper x bound of key picture
      REAL XCH                   ! Height of text with vertical baseline
      REAL XL                    ! X co-ordinate of left of text
      REAL XM                    ! X extent of key picture, in metres
      REAL XV                    ! X centre of vector
      REAL Y1                    ! Lower y bound of key picture
      REAL Y2                    ! Upper y bound of key picture
      REAL YC                    ! Y coord. of centre of key object
      REAL YM                    ! Y extent of key picture, in metres

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if the vector scale is zero.
      IF ( VSCALE .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POL1_VECKY_ZEROSCA', 'POL1_VECKY: Zero vector '/
     :                 /'scale supplied (programming error).', STATUS )
         GO TO 999
      END IF

*  Set the PGPLOT viewport and window to match the current AGI picture,
*  and get the extent of the window in world coordinates, and metres.
      CALL KPG1_GDQPC( X1, X2, Y1, Y2, XM, YM, STATUS )

*  See if a Title is to be drawn for the key.
      DRTTL = AST_GETL( IPLOT, 'DRAWTITLE', STATUS )

*  See if a Title has been supplied. If so, get it.
      IF( AST_TEST( IPLOT, 'TITLE', STATUS ) ) THEN
         VSCTXT = AST_GETC( IPLOT, 'TITLE', STATUS )
         VSCNC = MAX( 1, CHR_LEN( VSCTXT ) )

*  Note we do not need to draw the "Vector scale" string.
         DRTOP = .FALSE.

*  Otherwise construct a default title, giving the scale as a number ofd
*  data units per centimetre.
      ELSE

*  Note we do need to draw the "Vector scale" string.
         DRTOP = .TRUE.

*  Format the vector scale value (data units per centimetre), using the Plot's
*  1st axis format attribute.
         VSCTXT = AST_FORMAT( IPLOT, 1, DBLE( VSCALE ), STATUS )
         VSCNC = CHR_LEN( VSCTXT )

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

      END IF

*  Find the data value corresponding to a vector length equal to the
*  largest allowed fraction of the width of the key picture.  If the
*  supplied data value is smaller than this, use it.
      KEYDAT = ABS( VECMAX * 100.0 * XM * VSCALE )
      IF ( KDATA .GT. 0.0 .AND. KDATA .LT. KEYDAT ) KEYDAT = KDATA

*  Also ensure that the vector length is not too small.
      KEYDAT = MAX( KEYDAT, ABS( VECMIN * 100.0 * XM * VSCALE ) )

*  This data value will not in general be a round number.  Find a
*  pleasing round data value which produces a similar vector length.
*  BASE is assigned the power of ten just less than the initial value of
*  KEYDAT.
      BASE = 10.0**INT( LOG10( KEYDAT ) )
      IF ( KEYDAT .LT. 1.0 ) BASE = BASE * 0.1
      KEYDAT = REAL( MAX( 1, INT( KEYDAT/BASE ) ) ) * BASE

*  Get a new value, limiting it to 95% of the width of the key picture.
*  The value just calculated is used as the dynamic default.
      CALL PAR_GDR0R( PARKEY, KEYDAT, 0.0, 95.0 * XM * VSCALE, .FALSE.,
     :                KEYDAT, STATUS )

*  Format the data value into a character string, using the Plot's 2nd
*  axis format attribute.
      KEYTXT = AST_FORMAT( IPLOT, 2, DBLE( KEYDAT ), STATUS )
      KEYNC = CHR_LEN( KEYTXT )

*  Increment the position of the last significant character in KEYTXT in
*  order to produce a space between the value and the units string.
      KEYNC = KEYNC + 1

*  Decide how much of the units string will appear in the key caption.
*  Get the length of the UNITS.  The allowed length in the captions is
*  32 characters less the space and the value.
      IF ( UNITS .NE. ' ' ) THEN
         MXUNIT = 32 - KEYNC
         CUNITS = CHR_LEN( UNITS )

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

*  Store the textlabgap value specified by the key style. Use 1.0 if no
*  value has been specified.
      IF( AST_TEST( IPLOT, 'TEXTLABGAP', STATUS ) ) THEN
         GAP = AST_GETR( IPLOT, 'TEXTLABGAP', STATUS )
      ELSE
         GAP = 1.0
      END IF

*  Set the required PGPLOT character height. The actually text height
*  used will be "Size(strings)" (specified by KEYSTYLE) times this value.
      CALL PGSCH( HGTFAC )

*  Establish the style requested by the user for text.
      CALL KPG1_PGSTY( IPLOT, 'STRINGS', .TRUE., ATTS, STATUS )

*  Get the PGPLOT character height being used in world coordinates.
      CALL PGQCS( 4, XCH, HGT )

*  Note the Y value at the top of the key.
      YC = Y1 + KEYOFF*( Y2 - Y1 )
      TY = YC

*  Find the Y value at the bottom of the key.
      YC = YC - GAP * 3.0 * HGT

      IF( DRTTL ) THEN

         IF( DRTOP ) THEN
            YC = YC - GAP * 2.0 * HGT
         END IF

         YC = YC - GAP * 2.0 * HGT

      END IF

*  See if we can write in the background colour on the current device.
      CALL PGQCOL( CI1, CI2 )

*  If we can, clear the area covered by the key, but only if the key is
*  being drawn over the top of the vector map, and the text background
*  colour is not transparent..
      CALL GRF_GETTBG( TBG )
      IF( CLRKEY .AND. CI1 .EQ. 0 .AND. TBG .NE. -1 ) THEN

*  Save the current fill area attributes.
         CALL PGQFS( FS )
         CALL PGQCI( CI )

*  Set solid fill colour to pen TBG.
         CALL PGSFS( 1 )
         CALL PGSCI( TBG )

*  Draw a filled rectangle covering the key (slightly extended at top,
*  bottom and left).
         TY = TY + 0.05 * ( TY - YC )
         IF( TY .GT. 0.99*Y2 ) TY = 0.99*Y2
         YC = YC - 0.1 * ( TY - YC )
         CALL PGRECT( X1 - 0.05 * ( X2 - X1 ), X2, TY , YC )

*  Re-instate the original attributes.
         CALL PGSFS( FS )
         CALL PGSCI( CI )

      END IF

*  Set the Y world co-ordinate back to the top of the key.
      YC = Y1 + KEYOFF*( Y2 - Y1 )

*  Produce text describing the vector scale in words.  Left justify.
      XL = X1 + 0.01 * ( X2 - X1 )
      IF( DRTTL ) THEN

         IF( DRTOP ) THEN
            CALL PGTEXT( XL, YC - 0.5*HGT, 'Vector scale:' )
            YC = YC - GAP * 2.0 * HGT
         END IF

         CALL PGTEXT( XL, YC - 0.5*HGT, VSCTXT( : VSCNC ) )
         YC = YC - GAP * 2.0 * HGT

      END IF

*  Find the length of the key vector, in the world co-ordinate system
*  of the key picture.
      KEYLEN = ( X2 - X1 ) * KEYDAT / ( VSCALE * XM * 100.0 )

*  Convert the arrowhead size from metres to key-picture world
*  co-ordinates.
      AHSIZE = ( X2 - X1 ) * AHSIZM / XM

*  Revert to the previous PGPOLOT attribute settings, and then establish the
*  style requested by the user for lines.
      CALL KPG1_PGSTY( IPLOT, 'STRINGS', .FALSE., ATTS, STATUS )
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTS, STATUS )

*  Define the y position of the vector, and the radius of the circle
*  indicating the justification.  Usually draw the vector justified to
*  the left, but allowing room for the circle when the justification is
*  left justified too. Draw the vector centre justified if the key is
*  drawn on top of the vector map.
      YC = YC - GAP * HGT
      RADIUS = MIN( 0.1 * KEYLEN, ( X2 - X1 ) * CIRAD )

      IF( CLRKEY ) THEN
         XV = 0.5*( X1 + X2 )
      ELSE IF ( JUST .EQ. 'START' ) THEN
         XV = XL + 0.5 * KEYLEN + RADIUS
      ELSE
         XV = XL + 0.5 * KEYLEN
      END IF

      CALL POL1_VECT( XV, YC, 'CENTRE', KEYLEN, -90.0 * DTOR, AHSIZE,
     :                STATUS )

*  Draw a hollow circle to mark the vector reference position.
      CALL PGQFS( FS0 )
      CALL PGSFS( 2 )
      IF ( JUST .EQ. 'CENTRE' ) THEN
         CALL PGCIRC( XV, YC, RADIUS )

      ELSE IF ( JUST .EQ. 'START' ) THEN
         CALL PGCIRC( XV - 0.5 * KEYLEN, YC, RADIUS )

      ELSE
         CALL PGCIRC( XV + 0.5 * KEYLEN, YC, RADIUS )

      END IF
      CALL PGSFS( FS0 )

*  Revert to the previous PGPOLOT attribute settings, and then establish the
*  style requested by the user for text.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTS, STATUS )
      CALL KPG1_PGSTY( IPLOT, 'STRINGS', .TRUE., ATTS, STATUS )

*  Plot the text below the vector.
      IF( CLRKEY ) THEN
         CALL PGTEXT( XV - 0.3*HGT*KEYNC, YC - GAP * 2.0 * HGT,
     :                KEYTXT( : KEYNC ) )
      ELSE
         CALL PGTEXT( XL, YC - GAP * 2.0 * HGT, KEYTXT( : KEYNC ) )
      END IF

*  Revert to the previous PGPOLOT attribute settings.
      CALL KPG1_PGSTY( IPLOT, 'STRINGS', .FALSE., ATTS, STATUS )

*  Flush the graphics.
      CALL PGUPDT

*  Arrive here if an error has occurred.
 999  CONTINUE

      END
