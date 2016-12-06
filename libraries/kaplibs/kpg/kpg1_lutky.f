      SUBROUTINE KPG1_LUTKY( IPIC, PARAM, HIGH, LOW, LABEL, APP,
     :                       LP, UP, F, GAP1, GAP2, JUST, RJUST, NEL,
     :                       COLDAT, STATUS )
*+
*  Name:
*     KPG1_LUTKY

*  Purpose:
*     Draws a key showing a colour table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LUTKY( IPIC, PARAM, HIGH, LOW, LABEL, APP, LP, UP, F,
*                      GAP1, GAP2, JUST, RJUST, NEL, COLDAT, STATUS )

*  Description:
*     The key consists of a ramp of colour covering the specified range
*     of colour indices, with annotated axes.  Axis 1 is always the
*     data-value axis (whether it is drawn vertically or horizontally).
*     The whole plot (including annotation) is scaled to fit inside the
*     picture specified by IPIC.

*  Arguments:
*     IPIC = INTEGER (Given)
*        An AGI identifier for a picture in which the key is to be
*        produced.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the style parameter to use (e.g. STYLE, KEYSTYLE,
*        etc.).
*     HIGH = REAL (Given)
*        The X-axis value (i.e. pixel value or pen number) corresponding
*        to the colour index UP.
*     LOW = REAL (Given)
*        The X-axis value (i.e. pixel value or pen number) corresponding
*        to the colour index LP.
*     LABEL = CHARACTER * ( * ) (Given)
*        The default label to put against the data values.
*     APP = CHARACTER * ( * ) (Given)
*        The calling application, in the form "KAPPA_LUTVIEW".
*     LP = INTEGER (Given)
*        The smallest colour index to include in the display.
*     UP = INTEGER (Given)
*        The largest colour index to include in the display.
*     F = REAL (Given)
*        An amount by which to extend the margins left for annotation,
*        expressed as a factor of the height or width of the plotting
*        area.  For instance, a value of 0.1 could be given to fit the
*        annotation "comfortably" into the Plot.  A value of 0.0 will
*        result in the annotation being hard up against the edge of the
*        plot.
*     GAP1 = REAL (Given)
*        A gap, in millimetres, to place between the bottom or right
*        edge of the supplied picture, and the nearest edge of the
*        colour ramp.
*     GAP2 = REAL (Given)
*        A gap, in millimetres, to place between the top or left edge
*        of the supplied picture, and the nearest edge of the colour
*        ramp.
*     JUST = CHARACTER*2 (Given)
*        Indicates the justification of the new plot within the specified
*        area.  'BL', 'BC', 'BR', 'CL', 'CC', 'CR', 'TL', 'TC' or 'TR',
*        where B is Bottom, C is Centre, T is Top, L is Left and R is
*        Right. Must be upper case. If either character is a space, then
*        the corresponding value from RJUST is used instead. Other
*        unrecognised values are treated as "C".
*     RJUST( 2 ) = REAL (Given)
*        Each element is used only if the corresponding element in JUST
*        is a apce. The first element gives the fractional vertical position
*        of the new plot: 0.0 means put the new plot as low as possible, 1.0
*        means put it as high as possible. The second element gives the
*        fractional horizontal position of the new plot: 0.0 means put the
*        new plot as far to the left as possible, 1.0 means put it as far
*        to the right as possible.
*     NEL = INTEGER (Given)
*        The size of the vector COLDAT. If this is zero, COLDAT is not
*        accessed, and requested for keys in the form of a histogram
*        are ignored (that is, keys are always produced in the form of a
*        ramp).
*     COLDAT( NEL ) = INTEGER (Given)
*        A vector of colour indices, one for each displayed data pixel.
*        Only accessed if NEL is greater than zero and the STYLE
*        parameter indicates that the key is to drawn in the form of a
*        histogram.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1999 (DSB):
*        Original version.
*     23-OCT-2001 (DSB):
*        Added histogram and graph forms for key.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL.
*     2006 February 24 (MJC):
*        Added new CUMUL argument set to .FALSE. to KPG1_GHSTx call.
*     6-DEC-2016 (DSB):
*        Added argument RJUST.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IPIC
      CHARACTER PARAM*(*)
      REAL HIGH
      REAL LOW
      CHARACTER LABEL*(*)
      CHARACTER APP*(*)
      INTEGER LP
      INTEGER UP
      REAL F
      REAL GAP1
      REAL GAP2
      CHARACTER JUST*2
      REAL RJUST( 2 )
      INTEGER NEL
      INTEGER COLDAT( NEL )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Constants:
      REAL ASPNOM                ! Nominal aspect ratio for the colour
      PARAMETER ( ASPNOM = 12.0 ) ! ramp

      INTEGER RAMP               ! Symbolic constant for RAMP key form
      PARAMETER ( RAMP = 0 )

      INTEGER HIST               ! Symbolic constant for HIST key form
      PARAMETER ( HIST = 1 )

      INTEGER GRAPH              ! Symbolic constant for GRAPH key form
      PARAMETER ( GRAPH = 2 )

*  Local Variables:
      CHARACTER CLABEL*80        ! Label to use for Axis 2
      CHARACTER CSYM*10          ! Symbol to use for Axis 2
      CHARACTER DOM*30           ! Domain name
      CHARACTER EDGE1*6          ! Default edge for data value labels
      CHARACTER EDGE2*6          ! Default edge for other axis labels
      CHARACTER ULABEL*80        ! Label to use for Axis 1
      CHARACTER USYM*10          ! Symbol to use for Axis 1
      DOUBLE PRECISION INA( 2 )  ! GRAPHICS Frame co-ords at corner A
      DOUBLE PRECISION INB( 2 )  ! GRAPHICS Frame co-ords at corner B
      DOUBLE PRECISION OUTA( 2 ) ! Current Frame co-ords at corner A
      DOUBLE PRECISION OUTB( 2 ) ! Current Frame co-ords at corner B
      INTEGER CMAP1              ! Pointer to an AST Mapping
      INTEGER CMAP2              ! Pointer to an AST Mapping
      INTEGER CMAP3              ! Pointer to an AST Mapping
      INTEGER FORM               ! Form of key
      INTEGER FRM1               ! Pointer to an AST Frame
      INTEGER FRM2               ! Pointer to an AST Frame
      INTEGER FRM3               ! Pointer to an AST Frame
      INTEGER FRM4               ! Pointer to an AST Frame
      INTEGER FRM5               ! Pointer to an AST Frame
      INTEGER IPENY              ! Index of the PEN-Y Frame within IPLOT
      INTEGER IPHIST             ! Pointer to histogram of colour
                                 ! indices
      INTEGER IPICB              ! AGI id for BASE picture
      INTEGER IPLOT              ! Pointer to an AST Plot
      INTEGER IPRGB              ! Pointer to work array
      INTEGER IPWORK             ! Pointer to work array
      INTEGER IPX                ! Pointer to work array
      INTEGER LLEN               ! Length of label
      INTEGER MAP1               ! Pointer to an AST Mapping
      INTEGER MAP2               ! Pointer to an AST Mapping
      INTEGER MAP3               ! Pointer to an AST Mapping
      INTEGER MAP3A              ! Pointer to an AST Mapping
      INTEGER MAP3B              ! Pointer to an AST Mapping
      INTEGER MAP3C              ! Pointer to an AST Mapping
      INTEGER MAP3D              ! Pointer to an AST Mapping
      INTEGER MAP4               ! Pointer to an AST Mapping
      INTEGER MAP5               ! Pointer to an AST Mapping
      INTEGER MAXPOP             ! Largest count in colour index
                                 ! histogram
      INTEGER MAXPOS             ! Value of largest count in histogram
      INTEGER MINPOP             ! Smallest count in colour index
                                 ! histogram
      INTEGER MINPOS             ! Value of smallest count in histogram
      INTEGER NINVAL             ! Number of bad colour indices
      INTEGER NPEN               ! No. of pens in LUT
      INTEGER NZ                 ! No of pixels along Offset/Count axis
      INTEGER PERM( 6 )          ! Axis permutation array
      INTEGER PMAP               ! Pointer to an AST Mapping
      LOGICAL LOGPOP             ! Is count axis to be logarithmic?
      LOGICAL OK                 ! Could small Plot be made?
      LOGICAL PENNUM             ! Display pen numbers instead of data
                                 ! value?
      LOGICAL UPDATA             ! Use vertical edges for data axis?
      REAL ASPRAT                ! Aspect ratio for colour ramp
      REAL LBND( 2 )             ! Lower PEN-Y bounds
      REAL UBND( 2 )             ! Upper PEN-Y bounds
      REAL X1, X2, Y1, Y2        ! Bounds of PGPLOT window
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AGI context.
      CALL AGI_BEGIN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Re-create the PGPLOT viewport for the specified picture, and obtain
*  an AST pointer to the Plot associated with the picture.  World
*  co-ordinates in the viewport will be millimetres from the bottom left
*  of the view surface.
      CALL KPG1_GDGET( IPIC, AST__NULL, .FALSE., IPLOT, STATUS )

*  Get the dimensions of the viewport.
      CALL PGQWIN( X1, X2, Y1, Y2 )

*  Create a new Frame describing the displayed key.  This will be the
*  "annotation" Frame.  Axis 1 is data value or pen number, and Axis 2
*  is either count (for histogram keys), intensity (for graph keys), or
*  offset (for ramp keys).
      FRM5 = AST_FRAME( 2, ' ', STATUS )

*  Temporarily add this Frame into the Plot using a UnitMapping to
*  connect it the GRAPHICS (Base) Frame in the Plot.  The new Frame
*  becomes the current Frame.  This is done so that there is a Frame
*  which can be used to store the attribute settings supplied through
*  the STYLE parameter.  This Frame will be removed later and re-added
*  using apropriate Mappings.
      CALL AST_ADDFRAME( IPLOT, AST__BASE, AST_UNITMAP( 2, ' ',
     :                                                  STATUS ),
     :                   FRM5, STATUS )

*  Set the default Axis 1 label and symbol.
      CALL AST_SETC( IPLOT, 'LABEL(1)', LABEL, STATUS )
      CALL AST_SETC( IPLOT, 'SYMBOL(1)', 'VALUE', STATUS )

*  Set up the default edge for labelling.  If the viewport is short and
*  wide, Axis 1 of the annotation Frame (the data value axis) is
*  displayed horizontally by default.  It is displayed vertically by
*  default if the  viewport is tall and thin.
      IF( ABS( Y1 - Y2 ) .GT. ABS( X2 - X1 ) ) THEN
         EDGE1 = 'LEFT'
         EDGE2 = 'BOTTOM'
      ELSE
         EDGE1 = 'BOTTOM'
         EDGE2 = 'LEFT'
      ENDIF

*  Store the default edge in the Plot.
      CALL AST_SETC( IPLOT, 'EDGE(1)', EDGE1, STATUS )
      CALL AST_SETC( IPLOT, 'EDGE(2)', EDGE2, STATUS )

*  By default, pen numbers are not shown.
      CALL AST_SETL( IPLOT, 'DIRECTION(1)', .FALSE., STATUS )

*  By default, the histogram count axis (if used) is linear.
      CALL AST_SETL( IPLOT, 'DIRECTION(2)', .FALSE., STATUS )

*  Allow the synonym FORM to be used in place of the AST attribute
*  DOMAIN during the following call to KPG1_ASSET.
      CALL KPG1_ASPSY( 'FORM', 'DOMAIN', STATUS )
      CALL KPG1_ASPSY( 'PENNUMS', 'DIRECTION(1)', STATUS )
      CALL KPG1_ASPSY( 'LOGPOP', 'DIRECTION(2)', STATUS )

*  Allow the user to specify the plotting style.
      CALL KPG1_ASSET( APP, PARAM, IPLOT, STATUS )

*  Clear the synonyms for AST attributes.
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  See if pen numbers are to be displayed instead of data values.
      PENNUM = AST_GETL( IPLOT, 'DIRECTION(1)', STATUS )
      IF( PENNUM ) THEN
         USYM = 'PEN'
      ELSE
         USYM = 'PIXVAL'
      END IF

*  See if count axis (if used) is to show count or log(count).
      LOGPOP = AST_GETL( IPLOT, 'DIRECTION(2)', STATUS )

*  If pen numbers are being displayed, and no label has been specified,
*  use the label "Pen number".
      ULABEL = AST_GETC( IPLOT, 'LABEL(1)', STATUS )
      LLEN = MAX( CHR_LEN( LABEL ), CHR_LEN( ULABEL ) )
      IF( PENNUM .AND. ULABEL( : LLEN ) .EQ. LABEL( : LLEN ) ) THEN
         ULABEL = 'Pen Number'
      END IF

*  See what form of key (histogram, graph or ramp) is to be produced.
      DOM = AST_GETC( IPLOT, 'DOMAIN', STATUS )
      IF( INDEX( 'GRAPH', DOM( : CHR_LEN( DOM ) ) ) .EQ. 1 ) THEN
         FORM = GRAPH

      ELSE IF( INDEX( 'HISTOGRAM', DOM( : CHR_LEN( DOM ) ) )
     :         .EQ. 1 ) THEN
         FORM = HIST

      ELSE
         FORM = RAMP

      END IF

      IF( FORM .EQ. HIST .AND. NEL .EQ. 0 ) THEN
         FORM = RAMP
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', '  Cannot produce a histogram LUT key as '//
     :                 'no data array has been specified.', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

*  Get the edges on which the user wants labels to be put (he may have
*  over-ridden the default established above).
      EDGE1 = AST_GETC( IPLOT, 'EDGE(1)', STATUS )
      EDGE2 = AST_GETC( IPLOT, 'EDGE(2)', STATUS )
      CALL CHR_UCASE( EDGE1 )
      CALL CHR_UCASE( EDGE2 )

*  Store the number of pens.
      NPEN = UP - LP + 1

*  Store the pen number bounds of the area enclosing the key.
      LBND( 1 ) = REAL( LP ) - 0.5
      UBND( 1 ) = REAL( UP ) + 0.5

*  If a histogram key is being produced, form a histogram of the
*  supplied colour indices.
      IF( FORM .EQ. HIST ) THEN

*  Allocate memory for the histogram.
         CALL PSX_CALLOC( NPEN, '_INTEGER', IPHIST, STATUS )

*  Produce the histogram.
         CALL KPG1_GHSTI( .FALSE., NEL, COLDAT, COLDAT, 0.0D0, NPEN,
     :                    .FALSE., UP, LP, %VAL( CNF_PVAL( IPHIST ) ),
     :                    STATUS )

*  Find the maximum and minimum count in any bin.
         CALL KPG1_MXMNI( .FALSE., NPEN, %VAL( CNF_PVAL( IPHIST ) ),
     :                    NINVAL, MAXPOP,
     :                    MINPOP, MAXPOS, MINPOS, STATUS )

*  Store the axis-2 (count or log(count)) bounds of the key.  Also store
*  the label for the cross axis.
         LBND( 2 ) = 0.0
         IF( LOGPOP ) THEN
            UBND( 2 ) = 1.1*LOG10( REAL( MAXPOP ) )
            CLABEL = 'Log10( count )'
            CSYM = 'LOG(COUNT)'
         ELSE
            UBND( 2 ) = 1.1*REAL( MAXPOP )
            CLABEL = 'Count'
            CSYM = 'COUNT'
         END IF

*  If a graph or ramp key is being produced,
      ELSE

*  Store the Axis 2 (intensity or offset) bounds of the key.
         LBND( 2 ) = 0.0
         UBND( 2 ) = 1.05

*  Initialise the histogram pointer to zero.
         IPHIST = 0

*  Store the label for the cross axis.
         IF( FORM .EQ. GRAPH ) THEN
            CLABEL = 'Intensity'
            CSYM = 'INTENSITY'
         ELSE
            CLABEL = 'Offset'
            CSYM = 'OFFSET'
         END IF

      END IF

*  We now re-create the Plot adding in suitable Mappings and Frames.
*  First remove the Frame temporarily added above.
      CALL AST_REMOVEFRAME( IPLOT, AST__CURRENT, STATUS )

*  If the key is to be displayed so that the left or right edge is
*  annotated with data value/pen number, then we need to create a
*  Frame which is an axis swapped version of the GRAPHICS Frame. It
*  is given Domain "GRSWAP".  First get the indices of the GRAPHICS
*  Frame axis in the order required in the new Frame.
      IF( EDGE1 .EQ. 'LEFT' .OR. EDGE1 .EQ. 'RIGHT' ) THEN
         UPDATA = .TRUE.
         PERM( 1 ) = 2
         PERM( 2 ) = 1
         Y1 = MIN( Y2, Y1 + GAP1 )
         Y2 = MAX( Y1, Y2 - GAP2 )

*  Check that the Axis 2 labelling edge is not left or right.
         IF( EDGE2 .EQ. 'LEFT' .OR. EDGE2 .EQ. 'RIGHT' ) THEN
            EDGE2 = 'BOTTOM'
         END IF

      ELSE
         UPDATA = .FALSE.
         PERM( 1 ) = 1
         PERM( 2 ) = 2
         X1 = MIN( X2, X1 + GAP1 )
         X2 = MAX( X1, X2 - GAP2 )

*  Check that the Axis 2 labelling edge is not top or bottom.
         IF( EDGE2 .EQ. 'TOP' .OR. EDGE2 .EQ. 'BOTTOM' ) THEN
            EDGE2 = 'LEFT'
         END IF

      END IF

*  Create the new Frame, and get a Mapping (MAP1) form the GRAPHICS
*  Frame to the new Frame.
      FRM1 = AST_PICKAXES( AST_GETFRAME( IPLOT, AST__BASE, STATUS ), 2,
     :                     PERM, MAP1, STATUS )

*  Set the attributes for the new Frame.
      CALL AST_SETC( FRM1, 'DOMAIN', 'GRSWAP', STATUS )
      CALL AST_SETC( FRM1, 'LABEL(1)', 'Graphics position along the '//
     :               'colour table key', STATUS )
      CALL AST_SETC( FRM1, 'LABEL(2)', 'Graphics position across the '//
     :               'colour table key', STATUS )
      CALL AST_SETC( FRM1, 'TITLE', 'Graphics position within the '//
     :               'colour table key', STATUS )

*  Add the new Frame into the Plot, connecting it to the Base (GRAPHICS)
*  Frame using the Mapping returned by AST_PICKAXES.
      CALL AST_ADDFRAME( IPLOT, AST__BASE, AST_SIMPLIFY( MAP1, STATUS ),
     :                   FRM1, STATUS )

*  The next Frame to add has axes corresponding to pen number and
*  Y value (count.intensity or offset). This has Domain "PEN-Y", and
*  is connected to the GRSWAP Frame with a WinMap.
      IF( UPDATA ) THEN
         INA( 1 ) = Y1
         INA( 2 ) = X1
         INB( 1 ) = Y2
         INB( 2 ) = X2
      ELSE
         INA( 1 ) = X1
         INA( 2 ) = Y1
         INB( 1 ) = X2
         INB( 2 ) = Y2
      END IF

      OUTA( 1 ) = DBLE( LBND( 1 ) )
      OUTA( 2 ) = DBLE( LBND( 2 ) )
      OUTB( 1 ) = DBLE( UBND( 1 ) )
      OUTB( 2 ) = DBLE( UBND( 2 ) )

      MAP2 = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Create a two-dimensional Frame. Specify a format for Axis 1, which
*  results in pen numbers being displayed as integers.
      FRM2 = AST_FRAME( 2, 'DOMAIN=PEN-Y,'//
     :                     'FORMAT(1)=%.0f,'//
     :                     'LABEL(1)=Pen Number,'//
     :                     'SYMBOL(1)=PEN', STATUS )

*  Set other Frame attributes.
      CALL AST_SETC( FRM2, 'Label(2)', CLABEL, STATUS )
      CALL AST_SETC( FRM2, 'Symbol(2)', CSYM, STATUS )

*  Add the Frame to the Plot.
      CALL AST_ADDFRAME( IPLOT, AST__CURRENT,
     :                   AST_SIMPLIFY( MAP2, STATUS ), FRM2, STATUS )

*  Note the index of the PEN-Y Frame.
      IPENY = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  The next Frame has six axes; X-axis value (pen number or pixel
*  value), Y-axis value (offset, count or intensity), R intensity,
*  G intensity, B intensity, and pen number.

*  Create a one-dimensional UnitMap or WinMap from Pen number to X-axis
*  value.
      IF( PENNUM ) THEN
         MAP3A = AST_UNITMAP( 1, ' ', STATUS )
      ELSE
         INA( 1 ) = DBLE( LP ) - 0.5
         INB( 1 ) = DBLE( UP ) + 0.5
         OUTA( 1 ) = DBLE( LOW )
         OUTB( 1 ) = DBLE( HIGH )
         MAP3A = AST_WINMAP( 1, INA, INB, OUTA, OUTB, ' ', STATUS )
      END IF

*  Create a one-dimensional unit map to propagate the Y-axis value.
      MAP3B = AST_UNITMAP( 1, ' ', STATUS )

*  Compound them in parallel.
      CMAP1 = AST_CMPMAP( MAP3A, MAP3B, .FALSE., ' ', STATUS )

*  Create a Mapping from pen number to RGB intensities (one input, three
*  outputs).
      CALL PSX_CALLOC( 3 * NPEN, '_DOUBLE', IPWORK, STATUS )
      CALL KPG1_LUTK4( LP, UP, %VAL( CNF_PVAL( IPWORK ) ),
     :                 MAP3C, STATUS )
      CALL PSX_FREE( IPWORK, STATUS )

*  Add it to the parallel compound Mapping.
      CMAP2 = AST_CMPMAP( CMAP1, MAP3C, .FALSE., ' ', STATUS )

*  Create a one-dimensional unit map to propagate the pen number value.
      MAP3D = AST_UNITMAP( 1, ' ', STATUS )

*  Add it to the parallel compound Mapping.
      CMAP3 = AST_CMPMAP( CMAP2, MAP3D, .FALSE., ' ', STATUS )

*  Create a two-input, four-output PermMap to feed the four inputs of
*  the above parallel CmpMap.
      PERM( 1 ) = 1
      PERM( 2 ) = 2
      PERM( 3 ) = 1
      PERM( 4 ) = 1
      PMAP = AST_PERMMAP( 2, PERM, 4, PERM, 0.0D0, ' ', STATUS )

*  Combine this PermMap in series with the above parallel CmpMap.  This
*  is the final Mapping with two inputs and six outputs.
      MAP3 = AST_CMPMAP( PMAP, CMAP3, .TRUE., ' ', STATUS )

*  Create a 6D Frame to use,
      FRM3 = AST_FRAME( 6, 'DOMAIN=LUT,'//
     :                     'LABEL(3)=Red Intensity,'//
     :                     'LABEL(4)=Green Intensity,'//
     :                     'LABEL(5)=Blue Intensity,'//
     :                     'LABEL(6)=Pen Number,'//
     :                     'FORMAT(6)=%.0f,'//
     :                     'SYMBOL(3)=RED,'//
     :                     'SYMBOL(4)=GREEN,'//
     :                     'SYMBOL(5)=BLUE,'//
     :                     'SYMBOL(6)=PEN', STATUS )

*  Set Symbols and Labels for the first 2 axes.
      CALL AST_SETC( FRM3, 'LABEL(1)', ULABEL, STATUS )
      CALL AST_SETC( FRM3, 'LABEL(2)', CLABEL, STATUS )
      CALL AST_SETC( FRM3, 'SYMBOL(1)', USYM, STATUS )
      CALL AST_SETC( FRM3, 'SYMBOL(2)', CSYM, STATUS )

*  Add it into the Plot.
      CALL AST_ADDFRAME( IPLOT, AST__CURRENT,
     :                   AST_SIMPLIFY( MAP3, STATUS ), FRM3, STATUS )

*  The next Frame has 4 axes; X-axis value (pen number or pixel value),
*  R intensity, G intensity, B intensity. Pick these axes from the
*  previous Frame to create the new Frame.
      PERM( 1 ) = 1
      PERM( 2 ) = 3
      PERM( 3 ) = 4
      PERM( 4 ) = 5
      FRM4 = AST_PICKAXES( FRM3, 4, PERM, MAP4, STATUS )

*  Set the Domain in the new Frame.
      CALL AST_SETC( FRM4, 'DOMAIN', 'LUTRGB', STATUS )

*  Add the new Frame to the Plot.
      CALL AST_ADDFRAME( IPLOT, AST__CURRENT,
     :                   AST_SIMPLIFY( MAP4, STATUS ), FRM4, STATUS )

*  The final Frame has two axes; X-axis value (pen number or pixel
*  value), and Y-axis value (count, intensity or offset).  Pick these
*  axes from the LUT Frame to create the new Frame.
      PERM( 1 ) = 1
      PERM( 2 ) = 2
      FRM5 = AST_PICKAXES( FRM3, 2, PERM, MAP5, STATUS )

*  Set the Domain in the new Frame.
      CALL AST_SETC( FRM5, 'DOMAIN', 'LUTKEY', STATUS )

*  Add the new Frame to the Plot, feeding it from the IPENY Frame.  We
*  use the CMAP1 Mapping in preference to the MAP5 mapping.  This is
*  because MAP5 will have six inputs and two outputs and so will not
*  have an inverse.  On the other hand, CMAP1 has two inputs and two
*  outputs and will have an inverse (the AST routine which draws the
*  co-ordinate grid requires the
*  mapping to be invertable).
      CALL AST_ADDFRAME( IPLOT, IPENY,
     :                   AST_SIMPLIFY( CMAP1, STATUS ), FRM5, STATUS )

*  If we used the current Plot (IPLOT) to create the key, the key itself
*  (ramp, histogram or graph) would occupy the current PGPLOT viewport,
*  and any axis annotation would fall outside the current viewport.  We
*  want all components of the key, including annotation, to be within
*  the current viewport.  To achieve this, we "shrink" the Plot.  First,
*  get the desired aspect ratio for the new plot.  Zero means "make it
*  as big as possible".
      IF( FORM .EQ. RAMP ) THEN
         IF( UPDATA ) THEN
            ASPRAT = ASPNOM
         ELSE
            ASPRAT = 1.0/ASPNOM
         END IF
      ELSE
         ASPRAT = 0
      END IF

*  Replace the current Plot with a new Plot covering a smaller area so
*  that there is room for the annotation within the current viewport.
      CALL KPG1_ASSHR( ASPRAT, F, X1, X2, Y1, Y2, JUST, RJUST, IPLOT,
     :                 OK, STATUS )

*  Report an error if there was insufficient room to create the shrunken
*  Plot.
      IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_LUTKY_ERR1', 'There is insufficient room'//
     :                 ' for the colour table key.',
     :                 STATUS )
         GO TO 999
      END IF

*  By default, the other (non-data value) axis is annotated unless
*  ramp keys are being produced.
      CALL AST_SETL( IPLOT, 'NUMLAB(2)', (FORM .NE. RAMP), STATUS )
      CALL AST_SETL( IPLOT, 'TEXTLAB(2)', (FORM .NE. RAMP), STATUS )

*  Re-establish the attributes of the new Plot so that it is like the
*  old Plot.
      CALL KPG1_ASSET( APP, PARAM, IPLOT, STATUS )

*  Set the edges to label.
      CALL AST_SETC( IPLOT, 'EDGE(1)', EDGE1, STATUS )
      CALL AST_SETC( IPLOT, 'EDGE(2)', EDGE2, STATUS )

*  For ramp keys, do not allow the other axis to be annotated.
      IF( FORM .EQ. RAMP ) THEN
         CALL AST_SETL( IPLOT, 'NUMLAB(2)', .FALSE., STATUS )
         CALL AST_SETL( IPLOT, 'TEXTLAB(2)', .FALSE., STATUS )
      END IF

*  Save the Plot with the KEY picture in the graphics database.
      CALL KPG1_GDPUT( IPIC, ' ', ' ', IPLOT, STATUS )

*  Clear the vierport.
      CALL KPG1_PGCLR( STATUS )

*  If a graph key is being produced...
      IF( FORM .EQ. GRAPH ) THEN

*  Allocate work space and create the graph key.
         CALL PSX_CALLOC( 2*NPEN, '_DOUBLE', IPX, STATUS )
         CALL PSX_CALLOC( 3*NPEN, '_DOUBLE', IPRGB, STATUS )
         CALL KPG1_LUTK3( IPLOT, PARAM, APP, LP, UP,
     :                    %VAL( CNF_PVAL( IPX ) ),
     :                    %VAL( CNF_PVAL( IPRGB ) ), STATUS )
         CALL PSX_FREE( IPX, STATUS )
         CALL PSX_FREE( IPRGB, STATUS )

*  For Histogram and Ramp keys, the data are displayed as a pixmap
*  (image).
      ELSE

*  Find the number of pixels to use in the colour-index array in the
*  direction of the "other" axis.
         IF( FORM .EQ. HIST ) THEN

*  Find the bounds of the PGPLOT viewport in device pixels.
            CALL PGQVP( 3, X1, X2, Y1, Y2 )

*  For histogram keys, allow about 3 pixels per millimeter.
            IF( UPDATA ) THEN
               NZ = NINT( X2 - X1 ) + 1
            ELSE
               NZ = NINT( Y2 - Y1 ) + 1
            END IF

*  For ramp keys, just us an array one-pixel wide, set to the colour
*  index.
         ELSE IF( FORM .EQ. RAMP ) THEN
            NZ = 1

         END IF

*  Allocate the work array.
         CALL PSX_CALLOC( NZ*NPEN, '_INTEGER', IPWORK, STATUS )

*  If the data axis is vertical...
         IF( UPDATA ) THEN

*  Fill the colour table array.
            CALL KPG1_LUTK2( FORM, 2, 1, NZ, LP, UP,
     :                       %VAL( CNF_PVAL( IPHIST ) ),
     :                       MAXPOP, LOGPOP, %VAL( CNF_PVAL( IPWORK ) ),
     :                       STATUS )

*  Display it.
            CALL KPG1_PGPIX( IPLOT, 'PEN-Y', LBND, UBND, NZ, NPEN,
     :                       %VAL( CNF_PVAL( IPWORK ) ), STATUS )

*  If the data axis is horiontal...
         ELSE

*  Fill the colour table array.
            CALL KPG1_LUTK2( FORM, 1, LP, UP, 1, NZ,
     :                       %VAL( CNF_PVAL( IPHIST ) ),
     :                       MAXPOP, LOGPOP, %VAL( CNF_PVAL( IPWORK ) ),
     :                       STATUS )

*  Display it.
            CALL KPG1_PGPIX( IPLOT, 'PEN-Y', LBND, UBND, NPEN, NZ,
     :                       %VAL( CNF_PVAL( IPWORK ) ), STATUS )
         END IF

*  Free the work array.
         CALL PSX_FREE( IPWORK, STATUS )

      END IF

*  Draw the axes.
      CALL AGI_IBASE( IPICB, STATUS )
      CALL KPG1_ASGRD( IPLOT, IPICB, .TRUE., STATUS )
      CALL AGI_ANNUL( IPICB, STATUS )

*  Arrive here if an error has occurred.
 999  CONTINUE

*  If a histogram key was produced, free the histogram memory.
      IF( FORM .EQ. HIST ) CALL PSX_FREE( IPHIST, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  End the AGI context.
      CALL AGI_END( -1, STATUS )

      END
