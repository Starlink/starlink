      SUBROUTINE KPG1_LUTKY( IPIC, PARAM, HIGH, LOW, LABEL, APP, 
     :                       LP, UP, F, WORK, STATUS )
*+
*  Name:
*     KPG1_LUTKY

*  Purpose:
*     Draw a key showing a colour table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LUTKY( IPIC, PARAM, HIGH, LOW, LABEL, APP, LP, UP, F,
*                      WORK, STATUS )

*  Description:
*     The key consists of a ramp of colour covering the specified range
*     of colour indices, with annotated axes. Axis 1 is always the data 
*     value axis (whether it is drawn vertically or horizontally). The 
*     whole plot (including annotation) is scaled to fit inside the 
*     picture specified by IPIC.

*  Arguments:
*     IPIC = INTEGER (Given)
*        An AGI identifier for a picture in which the key is to be
*        produced.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the style parameter to use (eg STYLE, KEYSTYLE, etc).
*     HIGH = REAL (Given)
*        The data value corresponding to the colour index UP.
*     LOW = REAL (Given)
*        The data value corresponding to the colour index LP.
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
*        area. For instance, a value of 0.1 could be given to fit the 
*        annotation "comfortably" into the Plot. A value of 0.0 will 
*        result in the annotation being hard up against the edge of the 
*        plot.
*     WORK( LP : UP ) = REAL (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1999 (DSB):
*        Original version.
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
      INTEGER IPIC
      CHARACTER PARAM*(*)
      REAL HIGH
      REAL LOW
      CHARACTER LABEL*(*)
      CHARACTER APP*(*)
      INTEGER LP
      INTEGER UP
      REAL F

*  Arguments Returned:
      INTEGER WORK( LP : UP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER EDGE1*6          ! Default edge for data value labels
      CHARACTER EDGE2*6          ! Default edge for other axis labels
      DOUBLE PRECISION INA( 2 )  ! GRAPHICS Frame co-ords at corner A
      DOUBLE PRECISION INB( 2 )  ! GRAPHICS Frame co-ords at corner B
      DOUBLE PRECISION OUTA( 2 ) ! Current Frame co-ords at corner A
      DOUBLE PRECISION OUTB( 2 ) ! Current Frame co-ords at corner B
      INTEGER FRM                ! Pointer to an AST Frame
      INTEGER I                  ! Loop count   
      INTEGER IPICB              ! AGI id for BASE picture
      INTEGER IPLOT              ! Pointer to an AST Plot
      INTEGER MAP                ! Pointer to any AST Mapping
      INTEGER PERM( 2 )          ! Axis permutation array
      INTEGER PMAP               ! Pointer to an AST PermMap
      INTEGER WMAP               ! Pointer to an AST WinMap
      LOGICAL UPDATA             ! Use vertical edges for data axis?
      REAL LBND( 2 )             ! Lower LUTKEY bounds
      REAL UBND( 2 )             ! Upper LUTKEY bounds 
      REAL X1, X2, Y1, Y2        ! Bounds of PGPLOT window
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Re-create the PGPLOT viewport for the specified picture, and obtain an 
*  AST pointer to the Plot associated with the picture. World co-ordinates
*  in the viewport will be millimetres from the bottom left of the
*  view surface.
      CALL KPG1_GDGET( IPIC, AST__NULL, .FALSE., IPLOT, STATUS )

*  Create a new Frame describing the key. This will be the annotation
*  Frame.
      FRM = AST_FRAME( 2, 'DOMAIN=LUTKEY', STATUS )
      CALL AST_SETC( FRM, 'LABEL(1)', LABEL, STATUS )
      CALL AST_SETC( FRM, 'SYMBOL(1)', 'VALUE', STATUS )
      CALL AST_SETC( FRM, 'SYMBOL(2)', 'WIDTH', STATUS )

*  Add the annotation Frame into the Plot using a UnitMapping to connect it 
*  the GRAPHICS (Base) Frame in the Plot. The new Frame becomes the current 
*  Frame. This Frame will be re-mapped when it is known which edge
*  (vertical or horizontal) corresponds to data value.
      CALL AST_ADDFRAME( IPLOT, AST__BASE, AST_UNITMAP( 2, ' ', 
     :                                                  STATUS ), 
     :                   FRM, STATUS ) 

*  Get the dimensions of the viewport.
      CALL PGQWIN( X1, X2, Y1, Y2 )

*  Set up the default edge for labelling. If the viewport is short and 
*  wide, axis 1 of the annotation Frame (the data value axis) is displayed 
*  horizontally by default. It is displayed vertically by default if the 
*  viewport is tall and thin. 
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

*  Allow the user to specify the plotting style.
      CALL KPG1_ASSET( APP, PARAM, IPLOT, STATUS )

*  Get the edges on which the user wants labels to be put (he may have 
*  over-ridden the default established above).
      EDGE1 = AST_GETC( IPLOT, 'EDGE(1)', STATUS )
      EDGE2 = AST_GETC( IPLOT, 'EDGE(2)', STATUS )
      CALL CHR_UCASE( EDGE1 )
      CALL CHR_UCASE( EDGE2 )

*  Do not allow the other axis to be annotated.
      CALL AST_SETL( IPLOT, 'NUMLAB(2)', .FALSE., STATUS )
      CALL AST_SETL( IPLOT, 'TEXTLAB(2)', .FALSE., STATUS )

*  Store the bounds of the viewport in the annotated Frame.
      OUTA( 1 ) = DBLE( LOW )
      OUTA( 2 ) = 0.0D0
      OUTB( 1 ) = DBLE( HIGH )
      OUTB( 2 ) = 1.0D0

*  We now need to get the Mapping between the GRAPHICS Frame and the
*  annotation Frame. If the data value is being annotated on the top
*  or bottom edges, axis 1 of the annotation Frame (the data value axis) 
*  is displayed horizontally. It is displayed vertically if it is being
*  annotated on the left or right edges (in which case, create a PermMap 
*  which will swap the GRAPHICS axes).
      IF( EDGE1 .EQ. 'LEFT' .OR. EDGE1 .EQ. 'RIGHT' ) THEN
         UPDATA = .TRUE.

         PERM( 1 ) = 2
         PERM( 2 ) = 1
         PMAP = AST_PERMMAP( 2, PERM, 2, PERM, 0.0D0, ' ', STATUS ) 

*  Store the bounds of the viewport in the swapped GRAPHICS Frame.
         INA( 1 ) = DBLE( Y1 )
         INA( 2 ) = DBLE( X1 )
         INB( 1 ) = DBLE( Y2 )
         INB( 2 ) = DBLE( X2 )

*  Create a WinMap to map the swapped GRAPHICS Frame onto the annotation
*  Frame.
         WMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS ) 
        
*  Concatenate these two mapping to get the mapping from GRAPHICS Frame
*  to the annotation Frame.
         MAP = AST_CMPMAP( PMAP, WMAP, .TRUE., ' ', STATUS )

*  Check that the axis 2 labelling edge is not left or right.
         IF( EDGE2 .EQ. 'LEFT' .OR. EDGE2 .EQ. 'RIGHT' ) THEN
            EDGE2 = 'BOTTOM'
         END IF

*  If data value is to be annotated on bottom or top, we do not need to swap 
*  the GRAPHICS axes. Store the bounds of the viewport in the un-swapped 
*  GRAPHICS Frame.
      ELSE
         UPDATA = .FALSE.
         INA( 1 ) = DBLE( X1 )
         INA( 2 ) = DBLE( Y1 )
         INB( 1 ) = DBLE( X2 )
         INB( 2 ) = DBLE( Y2 )

*  Create a WinMap to map the GRAPHICS Frame onto the annotation Frame.
         MAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS ) 
        
*  Check that the axis 2 labelling edge is not top or bottom.
         IF( EDGE2 .EQ. 'TOP' .OR. EDGE2 .EQ. 'BOTTOM' ) THEN
            EDGE2 = 'LEFT'
         END IF

      ENDIF

*  Remap the annotation Frame, using the above Mapping.
      CALL AST_REMAPFRAME( IPLOT, AST__CURRENT, MAP, STATUS ) 

*  Replace the Plot with a new Plot covering a smaller area so that there
*  is room for the annotation within the current viewport.
      CALL KPG1_ASSHR( .FALSE., F, IPLOT, STATUS )

*  Re-establish the attributes of the new Plot so that it is like the old
*  Plot.
      CALL KPG1_ASSET( APP, PARAM, IPLOT, STATUS )

*  Set the egdes to label.
      CALL AST_SETC( IPLOT, 'EDGE(1)', EDGE1, STATUS )
      CALL AST_SETC( IPLOT, 'EDGE(2)', EDGE2, STATUS )

*  Do not allow the other axis to be annotated.
      CALL AST_SETL( IPLOT, 'NUMLAB(2)', .FALSE., STATUS )
      CALL AST_SETL( IPLOT, 'TEXTLAB(2)', .FALSE., STATUS )

*  Save the Plot with the KEY picture in the graphics database.
      CALL KPG1_GDPUT( IPIC, ' ', ' ', IPLOT, STATUS )

*  Set up the array of colour indices. 
      DO I = LP, UP
         WORK( I ) = I
      END DO

*  Store the bounds of the area enclosing the grey scale display, in
*  the annotation Frame.
      LBND( 1 ) = LOW
      LBND( 2 ) = 0.0
      UBND( 1 ) = HIGH
      UBND( 2 ) = 1.0

*  Display the colour table, vertically or horizontally as required.
      IF( EDGE1 .EQ. 'LEFT' .OR. EDGE1 .EQ. 'RIGHT' ) THEN
         CALL KPG1_PGPIX( IPLOT, 'LUTKEY', LBND, UBND, 1, UP - LP + 1, 
     :                    WORK, STATUS )
      ELSE
         CALL KPG1_PGPIX( IPLOT, 'LUTKEY', LBND, UBND, UP - LP + 1, 1,
     :                    WORK, STATUS )
      END IF

*  Draw the axes.
      CALL AGI_IBASE( IPICB, STATUS )
      CALL KPG1_ASGRD( IPLOT, IPICB, .TRUE., STATUS )
      CALL AGI_ANNUL( IPICB, STATUS )

*  Arrive here if an error has occurred.
 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

      END
