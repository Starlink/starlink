      SUBROUTINE KPS1_LSHPL( IPLOT, NPOS, NAX, POS, PLOT, GEO, IMARK, 
     :                       CLOSE, LABEL, IDS, WORK, STATUS )
*+
*  Name:
*     KPS1_LSHFM

*  Purpose:
*     Plot the positions selected by LISTSHOW.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LSHPL( IPLOT, NPOS, NAX, POS, PLOT, GEO, IMARK, CLOSE, 
*                      LABEL, IDS, WORK, STATUS )

*  Description:
*     This routine plots the supplied positions on the currently
*     opened PGPLOT device, using one of several different methods.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST Pointer to a Plot to perform the plotting.
*     NPOS = INTEGER (Given)
*        The number of supplied positions.
*     NAX = INTEGER (Given)
*        The number of axes for the supplied positions.
*     POS( NPOS, NAX ) = DOUBLE PRECISION (Given)
*        The supplied positions.
*     PLOT = CHARACTER * ( * ) (Given)
*        The type of plotting required. This can be:
*
*        MARK      - Each position is marked by symbol given by IMARK.
*        POLY      - Causes each position to be joined by a line
*                    (specified by GEO) to the previous position.  
*        CHAIN     - Each position is marked by symbol and joined to the
*                    previous position.
*     GEO = LOGICAL (Given)
*        Should geodesic polygons be drawn?
*     IMARK = INTEGER (Given)
*        PGPLOT marker type.
*     CLOSE = LOGICAL (Given)
*        Should polygons be closed?
*     LABEL = LOGICAL (Given)
*        Should positions be labelled?
*     IDS( NPOS ) = INTEGER (Given)
*        Array of position identifiers.
*     WORK( NPOS, 2 ) = DOUBLE PRECISION (Given and Returned)
*        Work space.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER IPLOT
      INTEGER NPOS
      INTEGER NAX
      DOUBLE PRECISION POS( NPOS, NAX )
      CHARACTER PLOT*(*)
      LOGICAL GEO
      INTEGER IMARK
      LOGICAL CLOSE
      LOGICAL LABEL
      INTEGER IDS( NPOS )

*  Arguments Given and Returned:
      DOUBLE PRECISION WORK( NPOS, 2 )

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      CHARACTER ID*6               ! Formatted identifier
      DOUBLE PRECISION DX          ! X position offset to label centre
      DOUBLE PRECISION DY          ! Y position offset to label centre
      DOUBLE PRECISION FINISH( NDF__MXDIM )! End of closing curve
      DOUBLE PRECISION NLG1        ! NumLabGap(1)
      DOUBLE PRECISION NLG2        ! NumLabGap(2)
      DOUBLE PRECISION SIZE        ! Size for numerical labels
      DOUBLE PRECISION START( NDF__MXDIM )! Start of closing curve
      DOUBLE PRECISION SZ0         ! Original size for strings
      DOUBLE PRECISION WD0         ! Original width for strings
      INTEGER CL0                  ! Original colour index for strings
      INTEGER FN0                  ! Original font for strings
      INTEGER FRM                  ! Pointer to current Frame
      INTEGER I                    ! Loop count
      INTEGER IAT                  ! No. of characters in a string
      INTEGER ICURR0               ! Index of original current frame
      INTEGER ICURR                ! Index of new current frame
      INTEGER J                    ! Axis index
      INTEGER ST0                  ! Original style for strings
      REAL UP(2)                   ! Up vector
      REAL X1, X2, Y1, Y2          ! Bounds of PGPLOT window (millimetres)
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Simplify the Plot. This adds a new Current Frame into the Plot, so note 
*  the index of the original Current Frame so that it can be re-instated later.
*  This can help to speed up the drawing, and also avoids the possibility
*  of the Mapping going via a Frame in which the positions are undefined.
      ICURR0 = AST_GETI( IPLOT, 'CURRENT', STATUS )
      CALL KPG1_ASSIM( IPLOT, STATUS )

*  Save the index of the new Current Frame.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Map the supplied positions into the GRAPHICS Frame.
      CALL AST_TRANN( IPLOT, NPOS, NAX, NPOS, POS, .FALSE., 2,
     :                NPOS, WORK, STATUS ) 

*  Now draw any required polygons.
      IF( PLOT .EQ. 'POLY' .OR. PLOT .EQ. 'CHAIN' ) THEN

*  For linear polygons, draw the poly-curve using the GRAPHICS co-ordinates 
*  stored in the work array.
         IF( .NOT. GEO ) THEN

*  Make the GRAPHICS (Base) Frame the Current Frame. Geodesics in this
*  Frame are straight lines on the screen.
            CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                                 STATUS ),
     :                     STATUS )

*  Draw the curve joining the GRAPHICS Frame positions.
            CALL AST_POLYCURVE( IPLOT, NPOS, 2, NPOS, WORK, STATUS )

*  If required, close the polygon.
            IF( CLOSE ) THEN
   
               DO I = 1, 2
                  START( I ) = WORK( NPOS, I )
                  FINISH( I ) = WORK( 1, I )
               END DO
   
               CALL AST_CURVE( IPLOT, START, FINISH, STATUS ) 
   
            END IF

*  Re-instate the Current Frame. 
            CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

*  For geodesic polygons, draw the poly-curve using the supplied Current 
*  Frame positions.
         ELSE 

*  Draw the poly-curve.
            CALL AST_POLYCURVE( IPLOT, NPOS, NAX, NPOS, POS, STATUS )

*  If required, close the polygon.
            IF( CLOSE ) THEN
   
               DO I = 1, NAX
                  START( I ) = POS( NPOS, I )
                  FINISH( I ) = POS( 1, I )
               END DO
   
               CALL AST_CURVE( IPLOT, START, FINISH, STATUS ) 
   
            END IF
   
         END IF

      END IF

*  If required, add labels.
      IF( LABEL .AND. STATUS .EQ. SAI__OK ) THEN

*  Make the GRAPHICS (Base) Frame the Current Frame. 
         CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                              STATUS ),
     :                  STATUS )

*  Get the size of the PGPLOT window.
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  Get the value of the NumLabGap value for each axis.
         NLG1 = AST_GETD( IPLOT, 'NUMLABGAP(1)', STATUS )
         NLG2 = AST_GETD( IPLOT, 'NUMLABGAP(2)', STATUS )

*  Get the scale factor for the size of numerical labels.
         SIZE = AST_GETD( IPLOT, 'SIZE(NUMLAB)', STATUS )

*  Find the offset in graphical co-ordinates from each position to the
*  centre of the label.
         DX = ABS( X2 - X1 )*NLG1*SIZE
         DY = ABS( Y2 - Y1 )*NLG2*SIZE

*  Draw text horizontally.
         UP( 1 ) = 0.0
         UP( 2 ) = 1.0

*  Temporarily set the attributes for text strings to be like Numerical
*  Labels.
         SZ0 = AST_GETD( IPLOT, 'SIZE(STRINGS)', STATUS )
         CL0 = AST_GETI( IPLOT, 'COLOUR(STRINGS)', STATUS )
         WD0 = AST_GETD( IPLOT, 'WIDTH(STRINGS)', STATUS )
         ST0 = AST_GETI( IPLOT, 'STYLE(STRINGS)', STATUS )
         FN0 = AST_GETI( IPLOT, 'FONT(STRINGS)', STATUS )

         CALL AST_SETD( IPLOT, 'SIZE(STRINGS)', 
     :                  AST_GETD( IPLOT, 'SIZE(NUMLAB)', STATUS ), 
     :                  STATUS )
         CALL AST_SETI( IPLOT, 'COLOUR(STRINGS)', 
     :                  AST_GETI( IPLOT, 'COLOUR(NUMLAB)', STATUS ), 
     :                  STATUS )
         CALL AST_SETD( IPLOT, 'WIDTH(STRINGS)', 
     :                  AST_GETD( IPLOT, 'WIDTH(NUMLAB)', STATUS ), 
     :                  STATUS )
         CALL AST_SETI( IPLOT, 'STYLE(STRINGS)', 
     :                  AST_GETI( IPLOT, 'STYLE(NUMLAB)', STATUS ), 
     :                  STATUS )
         CALL AST_SETI( IPLOT, 'FONT(STRINGS)', 
     :                  AST_GETI( IPLOT, 'FONT(NUMLAB)', STATUS ), 
     :                  STATUS )

*  Loop round each graphical co-ordinate position.
         DO J = 1, NPOS

*  Copy the co-ordinate values into an axis order array.
            DO I = 1, 2
               START( I ) = WORK( J, I )
            END DO

*  If the position is good...
            IF( START( 1 ) .NE. AST__BAD .AND. 
     :          START( 2 ) .NE. AST__BAD ) THEN         

*  Format the position identifier.
               ID = ' '
               IAT = 0
               CALL CHR_PUTI( IDS( J ), ID, IAT )

*  Detwermine the position for the centre of the label.
               START( 1 ) = START( 1 ) - DX
               START( 2 ) = START( 2 ) - DY

*  Draw the text.
               CALL AST_TEXT( IPLOT, ID( : IAT ), START, UP, 'CC', 
     :                        STATUS ) 

            END IF

         END DO

*  Reinstate the original STRINGS attributes.
         CALL AST_SETD( IPLOT, 'SIZE(STRINGS)', SZ0, STATUS )
         CALL AST_SETI( IPLOT, 'COLOUR(STRINGS)', CL0, STATUS )
         CALL AST_SETD( IPLOT, 'WIDTH(STRINGS)', WD0, STATUS )
         CALL AST_SETI( IPLOT, 'STYLE(STRINGS)', ST0, STATUS )
         CALL AST_SETI( IPLOT, 'FONT(STRINGS)', FN0, STATUS )

*  Re-instate the Current Frame. 
         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END IF

*  Finally draw any required markers using the GRAPHICS Frame positions.
      IF( PLOT .EQ. 'MARK' .OR. PLOT .EQ. 'CHAIN' ) THEN               

*  Make the GRAPHICS (Base) Frame the Current Frame. 
         CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                              STATUS ),
     :                  STATUS )

*  Draw the Markers.
         CALL AST_MARK( IPLOT, NPOS, 2, NPOS, WORK, IMARK, STATUS ) 

*  Re-instate the Current Frame. 
         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END IF

*  Remove the Current Frame added by KPG1_ASSIM and re-instate the original 
*  Current Frame.
      CALL AST_REMOVEFRAME( IPLOT, AST__CURRENT, STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', ICURR0, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
