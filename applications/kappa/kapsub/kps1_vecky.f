      SUBROUTINE KPS1_VECKY( PARKEY, IPLOT, VSCALE, AHSIZM, KEYOFF,
     :                       KDATA, UNITS, JUST, HGTFAC, STATUS )
*+
*  Name:
*     KPS1_VECKY

*  Purpose:
*     Draw a key for a vector map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_VECKY( PARKEY, IPLOT, VSCALE, AHSIZM, KEYOFF, KDATA, UNITS,
*                      JUST, HGTFAC, STATUS )

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
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
*     13-AUG-2002 (DSB):
*        Modified to include bounding box args and INK for KPS1_VECT.
*     {enter_further_changes_here}

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
      CHARACTER VSCTXT*80        ! Text describing scale
      DOUBLE PRECISION ATTS( 20 )! Saved graphics attribute values
      INTEGER CUNITS             ! Number of characters in units
      INTEGER FS0                ! Fill-area Style attribute on entry
      INTEGER KEYNC              ! Significant length of KEYTXT
      INTEGER MAXNC              ! Length of longest line of text
      INTEGER MXUNIT             ! Max. number of characters in units
      INTEGER VSCNC              ! Significant length of VSCTXT
      REAL AHSIZE                ! Arrowhead size in world coordinates
      REAL BASE                  ! Power of ten just less than KEYDAT
      REAL GAP                   ! Vertical spacing factor
      REAL HGT                   ! Character height in world co-ordinates
      REAL KEYDAT                ! Data value for key vector
      REAL KEYLEN                ! Vector length in centimetres
      REAL RADIUS                ! Used radius of the circle
      REAL X1                    ! Lower x bound of key picture
      REAL X2                    ! Upper x bound of key picture
      REAL XCH                   ! Height of text with vertical baseline
      REAL XL                    ! X co-ordinate of left of text
      REAL XM                    ! X extent of key picture, in metres
      REAL Y1                    ! Lower y bound of key picture
      REAL Y2                    ! Upper y bound of key picture
      REAL YC                    ! Y coord. of centre of key object
      REAL YM                    ! Y extent of key picture, in metres
      REAL DX1, DX2, DY1, DY2    ! Bounding box (unused)
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

*  Set the PGPLOT viewport and window to match the current AGI picture,
*  and get the extent of the window in world coordinates, and metres.
      CALL KPG1_GDQPC( X1, X2, Y1, Y2, XM, YM, STATUS )

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
      CALL PAR_GDR0R( PARKEY, KEYDAT, 0.0, 95.0 * XM * VSCALE, .TRUE.,
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

*  Set the Y world co-ordinate at the top of the key.
      YC = Y1 + KEYOFF*( Y2 - Y1 )

*  Produce text describing the vector scale in words.  Left justify.
      XL = X1 + 0.01 * ( X2 - X1 )
      CALL PGTEXT( XL, YC - 0.5*HGT, 'Vector scale:' )

      YC = YC - GAP * 2.0 * HGT
      CALL PGTEXT( XL, YC - 0.5*HGT, VSCTXT( : VSCNC ) )

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
*  indiciating the justification.  Draw the vector justified to the
*  left, but allowing room for the circle when the justification is
*  left justified too.
      YC = YC - GAP * 3.0 * HGT
      RADIUS = MIN( 0.1 * KEYLEN, ( X2 - X1 ) * CIRAD )

      IF ( JUST .EQ. 'START' ) THEN
         CALL KPS1_VECT( .TRUE., XL + 0.5 * KEYLEN + RADIUS, YC,
     :                   'CENTRE', KEYLEN, -90.0 * DTOR, AHSIZE, DX1,
     :                   DX2, DY1, DY2, STATUS )
      ELSE
         CALL KPS1_VECT( .TRUE., XL + 0.5 * KEYLEN, YC, 'CENTRE',
     :                   KEYLEN, -90.0 * DTOR, AHSIZE, DX1, DX2, DY1,
     :                   DY2, STATUS )
      END IF

*  Draw a hollow circle to mark the vector reference position.
      CALL PGQFS( FS0 )
      CALL PGSFS( 2 )
      IF ( JUST .EQ. 'CENTRE' ) THEN
         CALL PGCIRC( XL + 0.5 * KEYLEN, YC, RADIUS )

      ELSE IF ( JUST .EQ. 'START' ) THEN
         CALL PGCIRC( XL + RADIUS, YC, RADIUS )

      ELSE
         CALL PGCIRC( XL + 1.0 * KEYLEN, YC, RADIUS )
      END IF
      CALL PGSFS( FS0 )

*  Revert to the previous PGPOLOT attribute settings, and then establish the
*  style requested by the user for text.
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTS, STATUS )
      CALL KPG1_PGSTY( IPLOT, 'STRINGS', .TRUE., ATTS, STATUS )

*  Plot the text below the vector.
      CALL PGTEXT( XL, YC - GAP * 2.0 * HGT, KEYTXT( : KEYNC ) )

*  Revert to the previous PGPOLOT attribute settings.
      CALL KPG1_PGSTY( IPLOT, 'STRINGS', .FALSE., ATTS, STATUS )

*  Flush the graphics.
      CALL PGUPDT

*  Arrive here if an error has occurred.
 999  CONTINUE

      END
