      SUBROUTINE KPG1_ASSHR( FIXAR, IPLOT, STATUS )
*+
*  Name:
*     KPG1_PLOT

*  Purpose:
*     Shrink a Plot so that it covers an area which allows all annotation 
*     to fit within the current PGPLOT viewport.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSHR( FIXAR, IPLOT, STATUS )

*  Description:
*     This routine creates a new Plot covering the same window as the 
*     supplied Plot, but shrunk in GRAPHICS co-ordinates so that all the
*     annotation produced by AST_GRID falls within the PGPLOT viewport
*     which is current on entry.

*  Arguments:
*     FIXAR = LOGICAL (Given)
*        Should the aspect ratio of the window be retained? If not, the 
*        largest possible area is used for the new window.
*     IPLOT = INTEGER (Given and Returned)
*        The Plot. The supplied Plot is annulled and a new one is
*        returned in its place. The new Plot contains all the Frames of
*        the supplied Plot, but its Plot attributes are all cleared. It may
*        be necessary for the caller to re-instate these attributes.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-OCT-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      LOGICAL FIXAR

*  Arguments Given and Returned:
      INTEGER IPLOT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL CHR_SIMLR          ! Two strings equal apart from case?

*  Local constants :
      REAL ARAT                  ! Nominal character aspect ratio
      PARAMETER ( ARAT = 1.5 )

*  Local Variables:
      CHARACTER EDGE1*6          ! The edge to receive axis 1 labels
      CHARACTER EDGE2*6          ! The edge to receive axis 2 labels
      DOUBLE PRECISION BBOX( 4 ) ! Bounds of original window
      DOUBLE PRECISION XC        ! Axis 1 value
      DOUBLE PRECISION YC        ! Axis 2 value
      INTEGER ICURR              ! Current Frame index in old Plot
      INTEGER NC1                ! No. of characters in an axis 1 value
      INTEGER NC2                ! No. of characters in an axis 2 value
      INTEGER NCURR              ! Current Frame index in new Plot
      INTEGER NPLOT              ! New Plot
      LOGICAL EXTLAB             ! Are numerical labels drawn around edge?
      LOGICAL NUMLB1             ! Are numerical labels drawn for axis 1
      LOGICAL NUMLB2             ! Are numerical labels drawn for axis 2
      LOGICAL TKALL              ! Draw tick marks on all edges?
      LOGICAL TXTLB1             ! Are textual labels drawn for axis 1
      LOGICAL TXTLB2             ! Are textual labels drawn for axis 2
      REAL AR                    ! Original window aspect ratio
      REAL CEN                   ! Position of window centre
      REAL GBOX( 4 )             ! Bounds of new window
      REAL HGT                   ! Height of window 
      REAL HTHGT                 ! Height of text with horizontal baseline
      REAL HTWID                 ! Char. width of text with horizontal baseline
      REAL MBOT                  ! Width of margin at bottom for annotation
      REAL MINDIM                ! Length of minimum viewport dimension
      REAL MLEFT                 ! Width of margin at left for annotation
      REAL MRIGHT                ! Width of margin at right for annotation
      REAL MTOP                  ! Width of margin at top for annotation
      REAL NLGAP1                ! Gap between axis 1 and numerical labels
      REAL NLGAP2                ! Gap between axis 2 and numerical labels
      REAL NLSIZE                ! Character size for numerical labels
      REAL TKLEN                 ! Max (-ve) length of tick marks
      REAL TLGAP1                ! Gap between axis 1 and textual labels 
      REAL TLGAP2                ! Gap between axis 2 and textual labels 
      REAL TLSIZE                ! Character size for textual labels
      REAL VTHGT                 ! Height of text with vertical baseline
      REAL VTWID                 ! Char. width of text with vertical baseline
      REAL WID                   ! Width of window 
      REAL X1, X2, Y1, Y2    	 ! GRAPHICS bounds of PGPLOT viewport
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the bounds of the current PGPLOT window, in the GRAPHICS Frame 
*  (millimetres from the bottom left corner of the view surface).
      CALL PGQWIN( X1, X2, Y1, Y2 )

*  Store the length of the minimum dimension.
      MINDIM = MIN( ABS( X1 - X2 ), ABS( Y1 - Y2 ) )

*  If required, store the original aspect ratio.
      IF( FIXAR ) AR = ABS( Y1 - Y2 )/ABS( X1 - X2 )

*  Get the nominal text height and width in PGPLOT world co-ordinates.
*  Could be different for vertical and horizontal text.
      CALL PGQCS( 4, VTHGT, HTHGT )
      HTWID = HTHGT/ARAT
      VTWID = VTHGT/ARAT

*  Get various required Plot attributes.
      EXTLAB = CHR_SIMLR( AST_GETC( IPLOT, 'LABELLING', STATUS ),
     :                    'EXTERIOR' )
      NUMLB1 = AST_GETL( IPLOT, 'NUMLAB(1)', STATUS )
      NUMLB2 = AST_GETL( IPLOT, 'NUMLAB(2)', STATUS )
      TXTLB1 = AST_GETL( IPLOT, 'TEXTLAB(1)', STATUS )
      TXTLB2 = AST_GETL( IPLOT, 'TEXTLAB(2)', STATUS )
      EDGE1 = AST_GETC( IPLOT, 'EDGE(1)', STATUS )
      EDGE2 = AST_GETC( IPLOT, 'EDGE(2)', STATUS )
      TKLEN = MIN( AST_GETR( IPLOT, 'MAJTICKLEN', STATUS ),
     :             AST_GETR( IPLOT, 'MINTICKLEN', STATUS ) )
      TKALL = AST_GETL( IPLOT, 'TICKALL', STATUS )
      NLGAP1 = AST_GETR( IPLOT, 'NUMLABGAP(1)', STATUS )
      NLGAP2 = AST_GETR( IPLOT, 'NUMLABGAP(2)', STATUS )
      TLGAP1 = AST_GETR( IPLOT, 'TEXTLABGAP(1)', STATUS )
      TLGAP2 = AST_GETR( IPLOT, 'TEXTLABGAP(2)', STATUS )
      NLSIZE = AST_GETR( IPLOT, 'SIZE(NUMLAB)', STATUS )
      TLSIZE = AST_GETR( IPLOT, 'SIZE(TEXTLAB)', STATUS )

*  Transform the centre position into the current Frame.
      CALL AST_TRAN2( IPLOT, 1, 0.5*DBLE( X1 + X2 ), 
     :                0.5*DBLE( Y1 + Y2 ), .TRUE., XC, YC, STATUS ) 

*  Find the number of characters in a formatted axis 1 value.
      NC1 = CHR_LEN( AST_FORMAT( IPLOT, 1, XC, STATUS ) )

*  Find the number of characters in a formatted axis 2 value.
      NC2 = CHR_LEN( AST_FORMAT( IPLOT, 2, YC, STATUS ) )

*  Find the margin required for annotation at the bottom of the plot.
*  First, find the space needed for any tick marks on the bottom edge.
      MBOT = 0.0
      IF( TKALL .OR. CHR_SIMLR( EDGE1, 'BOTTOM' ) .OR. 
     :               CHR_SIMLR( EDGE2, 'BOTTOM' ) ) THEN
         IF( TKLEN .LT. 0.0 ) THEN
            MBOT = -MINDIM*TKLEN
         END IF
      END IF

*  If numerical labels are being drawn on the bottom edge, see how far
*  they extend from the edge of the data area.
      IF( EXTLAB ) THEN
         IF( NUMLB1 .AND. CHR_SIMLR( EDGE1, 'BOTTOM' ) ) THEN
            MBOT = MAX( MBOT, NLGAP1*MINDIM + NLSIZE*HTHGT )
         ELSE IF( NUMLB2 .AND. CHR_SIMLR( EDGE2, 'BOTTOM' ) ) THEN
            MBOT = MAX( MBOT, NLGAP2*MINDIM + NLSIZE*HTHGT )
         END IF
      END IF

*  If text labels are being drawn on the bottom edge, see how far
*  they extend.
      IF( TXTLB1 .AND. CHR_SIMLR( EDGE1, 'BOTTOM' ) ) THEN
         MBOT = MBOT + MAX( 0.0, TLGAP1*MINDIM + TLSIZE*HTHGT )
      ELSE IF( TXTLB2 .AND. CHR_SIMLR( EDGE2, 'BOTTOM' ) ) THEN
         MBOT = MBOT + MAX( 0.0, TLGAP2*MINDIM + TLSIZE*HTHGT )
      END IF

*  Now do the same for the top margin.
      MTOP = 0.0
      IF( TKALL .OR. CHR_SIMLR( EDGE1, 'TOP' ) .OR. 
     :               CHR_SIMLR( EDGE2, 'TOP' ) ) THEN
         IF( TKLEN .LT. 0.0 ) THEN
            MTOP = -MINDIM*TKLEN 
         END IF
      END IF

      IF( EXTLAB ) THEN
         IF( NUMLB1 .AND. CHR_SIMLR( EDGE1, 'TOP' ) ) THEN
            MTOP = MAX( MTOP, NLGAP1*MINDIM + NLSIZE*HTHGT )
         ELSE IF( NUMLB2 .AND. CHR_SIMLR( EDGE2, 'TOP' ) ) THEN
            MTOP = MAX( MTOP, NLGAP2*MINDIM + NLSIZE*HTHGT )
         END IF
      END IF

      IF( TXTLB1 .AND. CHR_SIMLR( EDGE1, 'TOP' ) ) THEN
         MTOP = MTOP + MAX( 0.0, TLGAP1*MINDIM + TLSIZE*HTHGT )
      ELSE IF( TXTLB2 .AND. CHR_SIMLR( EDGE2, 'TOP' ) ) THEN
         MTOP = MTOP + MAX( 0.0, TLGAP2*MINDIM + TLSIZE*HTHGT )
      END IF

*  Add on space to the top margin for the title if required.
      IF( AST_GETC( IPLOT, 'TITLE', STATUS ) .NE. ' ' .AND.
     :    AST_GETL( IPLOT, 'DRAWTITLE', STATUS ) ) THEN
         MTOP = MTOP + MAX( 0.0, 
     :                 MINDIM*AST_GETR( IPLOT, 'TITLEGAP', STATUS ) + 
     :                 HTHGT*AST_GETR( IPLOT, 'SIZE(TITLE)', STATUS ) )
      END IF

*  Now do the same for the left margin, but taking account of the fact
*  that numerical labels on the left and right are horizontal and so extend
*  the margin by their length rather than their height.
      MLEFT = 0.0
      IF( TKALL .OR. CHR_SIMLR( EDGE1, 'LEFT' ) .OR. 
     :               CHR_SIMLR( EDGE2, 'LEFT' ) ) THEN
         IF( TKLEN .LT. 0.0 ) THEN
            MLEFT = -MINDIM*TKLEN 
         END IF
      END IF

      IF( EXTLAB ) THEN
         IF( NUMLB1 .AND. CHR_SIMLR( EDGE1, 'LEFT' ) ) THEN
            MLEFT = MAX( MLEFT, NLGAP1*MINDIM + NLSIZE*HTWID*NC1 )
         ELSE IF( NUMLB2 .AND. CHR_SIMLR( EDGE2, 'LEFT' ) ) THEN
            MLEFT = MAX( MLEFT, NLGAP2*MINDIM + NLSIZE*HTWID*NC2 )
         END IF
      END IF

      IF( TXTLB1 .AND. CHR_SIMLR( EDGE1, 'LEFT' ) ) THEN
         MLEFT = MLEFT + MAX( 0.0, TLGAP1*MINDIM + TLSIZE*VTHGT )
      ELSE IF( TXTLB2 .AND. CHR_SIMLR( EDGE2, 'LEFT' ) ) THEN
         MLEFT = MLEFT + MAX( 0.0, TLGAP2*MINDIM + TLSIZE*VTHGT )
      END IF

*  Now do the same for the right margin.
      MRIGHT = 0.0
      IF( TKALL .OR. CHR_SIMLR( EDGE1, 'RIGHT' ) .OR. 
     :               CHR_SIMLR( EDGE2, 'RIGHT' ) ) THEN
         IF( TKLEN .LT. 0.0 ) THEN
            MRIGHT = -MINDIM*TKLEN 
         END IF
      END IF

      IF( EXTLAB ) THEN
         IF( NUMLB1 .AND. CHR_SIMLR( EDGE1, 'RIGHT' ) ) THEN
            MRIGHT = MAX( MRIGHT, NLGAP1*MINDIM + NLSIZE*HTWID*NC1 )
         ELSE IF( NUMLB2 .AND. CHR_SIMLR( EDGE2, 'RIGHT' ) ) THEN
            MRIGHT = MAX( MRIGHT, NLGAP2*MINDIM + NLSIZE*HTWID*NC2 )
         END IF
      END IF

      IF( TXTLB1 .AND. CHR_SIMLR( EDGE1, 'RIGHT' ) ) THEN
         MRIGHT = MRIGHT + MAX( 0.0, TLGAP1*MINDIM + TLSIZE*VTHGT )
      ELSE IF( TXTLB2 .AND. CHR_SIMLR( EDGE2, 'RIGHT' ) ) THEN
         MRIGHT = MRIGHT + MAX( 0.0, TLGAP2*MINDIM + TLSIZE*VTHGT )
      END IF

*  Find the bounds to use for the unclipped region. If no aspect ratio
*  has been specified, use the whole of the available space, excluding
*  the margins found above.
      IF( .NOT. FIXAR ) THEN
         GBOX( 1 ) = X1 + MLEFT
         GBOX( 3 ) = X2 - MRIGHT
         GBOX( 2 ) = Y1 + MBOT
         GBOX( 4 ) = Y2 - MTOP

*  If the original window had an aspect ratio greater than 1 (a tall
*  thin window), use the full height, and centre the area horizontally
*  between the left and right margins.
      ELSE IF( AR .GT. 1.0 ) THEN
         GBOX( 2 ) = Y1 + MBOT
         GBOX( 4 ) = Y2 - MTOP

         CEN = 0.5*( ( X1 + MLEFT ) + ( X2 - MRIGHT ) )
         WID = ( GBOX( 4 ) - GBOX( 2 ) )/AR
         GBOX( 1 ) = CEN - 0.5*WID
         GBOX( 3 ) = CEN + 0.5*WID

*  If the original window had an aspect ratio less than 1 (a short
*  wide window), use the full width, and centre the area vertically
*  between the top and bottom margins.
      ELSE 
         GBOX( 1 ) = X1 + MLEFT
         GBOX( 3 ) = X2 - MRIGHT

         CEN = 0.5*( ( Y1 + MBOT ) + ( Y2 - MTOP ) )
         HGT = ( GBOX( 3 ) - GBOX( 1 ) )*AR
         GBOX( 2 ) = CEN - 0.5*HGT
         GBOX( 4 ) = CEN + 0.5*HGT
      END IF

*  The graphics area covered by a Plot cannot be changed after it has been
*  created. Therefore, since we want a Plot covering a different area to
*  the original Plot, we have to create a new Plot covering the required 
*  area and then add all the original Plot Frames back into it. 

*  Create the new Plot holding the Base (GRAPHICS) Frame from the supplied 
*  Plot. The new Plot covers the screen area found above. This area is 
*  mapped into the entire area covered by the old Plot.
      BBOX( 1 ) = DBLE( X1 )
      BBOX( 2 ) = DBLE( Y1 )
      BBOX( 3 ) = DBLE( X2 )
      BBOX( 4 ) = DBLE( Y2 )
      NPLOT = AST_PLOT( AST_GETFRAME( IPLOT, AST__BASE, STATUS ), GBOX,
     :                  BBOX, ' ', STATUS ) 

*  We are now in the slightly odd situation of having two Frames with
*  Domain GRAPHICS in the Plot. Note the index of the one inherited from
*  the original Plot (i.e. the Current Frame). We will remove this Frame
*  later.
      NCURR = AST_GETI( NPLOT, 'CURRENT', STATUS )

*  Add the FrameSet representing the original Plot into this new Plot,
*  using a UnitMap to connect the two corresponding GRAPHICS Frames.
*  Note, the index of the Current Frame first so that it can be
*  reinstated later.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE', 
     :                                           STATUS ), STATUS )
      CALL AST_ADDFRAME( NPLOT, AST__CURRENT, AST_UNITMAP( 2, ' ', 
     :                                                     STATUS ),
     :                   IPLOT, STATUS ) 

*  We now have three GRAPHICS Frames in the Plot! Clear the Domain of the 
*  current Frame to avoid confusion.
      CALL AST_CLEAR( IPLOT, 'DOMAIN', STATUS )

*  There are now two copies of the original GRAPHICS Frame in the Plot
*  (i.e. the two Frames connected by the UnitMap). We can get rid of one.
*  Get rid of the one which was originally in the new Plot.
      CALL AST_REMOVEFRAME( NPLOT, NCURR, STATUS )

*  Re-instate the original Current Frame, allowing for the one extra Frame
*  introduced within the new Plot (there were two extra Frames but we've
*  just removed one of them).
      CALL AST_SETI( NPLOT, 'CURRENT', ICURR + 1, STATUS )

*  Annul the supplied Plot.
      CALL AST_ANNUL( IPLOT, STATUS )      

*  Export the returned Plot.
      IPLOT = AST_CLONE( NPLOT, STATUS )
      CALL AST_EXPORT( IPLOT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
