      SUBROUTINE KPG1_ASGRD( IPLOT, IPIC, GRID, STATUS )
*+
*  Name:
*     KPG1_ASGRD

*  Purpose:
*     Draw a border or an annotated coordinate grid over an AST Plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASGRD( IPLOT, IPIC, GRID, STATUS )

*  Description:
*     This routine call AST_BORDER to draw a border, or AST_GRID to draw an 
*     annotated coordinate Grid over the supplied Plot. The current pgplot 
*     viewport can optionally be extended prior to drawing the grid so that 
*     it covers a specified AGI picture. If the pgplot viewport is left 
*     matching the plotting area supplied when the Plot was created, then 
*     certain component of the grid (i.e. exterior tick marks), are clipped 
*     by pgplot. To avoid this, IPIC should normally be given as the AGI 
*     identifier for the FRAME picture containing the plot.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot. 
*     IPIC = INTEGER (Given)
*        An AGI identifier for the FRAME picture. Supply this as -1 if the
*        current pgplot viewport is not to be changed.
*     GRID = LOGICAL (Read)
*        Draw a grid using AST_GRID? If .FALSE. then a border only is drawn 
*        (using AST_BORDER).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The PGPLOT interface to the AGI library should be opened before
*     calling this routine.  

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1998 (DSB):
*        Original version.
*     20-SEP-2000 (DSB):
*        If AST_GRID or AST_BORDER fails, suggest that the user changes
*        current co-ordinate Frame.
*     17-AUG-2005 (DSB):
*        Modified to draw a second grid if the current Frame of the Plot
*        contains a DSBSpecFrame, showing the other side band.
*     2-DEC-2005 (DSB):
*        Correct "Title=0" to "DrawTitle=0".
*     16-DEC-2005 (DSB):
*        Allow user to prevent annotation of unused DSBSpecFrame axis
*        using the pseudo-attribute "DrawDSB".
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'AST_ERR'          ! AST error constants 

*  Arguments Given:
      INTEGER IPLOT
      INTEGER IPIC
      LOGICAL GRID

*  Status:
      INTEGER STATUS               ! Global status

*  External References:
      LOGICAL KPG1_GETASTDSB       ! Should both sidebands be annotated?

*  Local Variables:
      CHARACTER AEDGE*8            ! Name of Edge attribute to use
      CHARACTER AGAP*15            ! Value of TextGap attribute 
      CHARACTER ASB*8              ! Name of SideBand attribute to use
      CHARACTER TEXT*250           ! Attribute settings for second plot
      CHARACTER VEDGE*10           ! Value of Edge attribute 
      CHARACTER VSB*10             ! Value of SideBand attribute 
      DOUBLE PRECISION VGAP        ! Value for TitleGap
      INTEGER AX                   ! Pointer to an axis of Plot's current Frame
      INTEGER IAT                  ! Used length of TEXT
      INTEGER IPIC0                ! AGI id for original current picture
      INTEGER IPLOT2               ! Modified Plot
      INTEGER MAP                  ! Pointer to an unused Mapping
      LOGICAL BOX                  ! Was a simple box drawn?
      REAL A                       ! Scale factor
      REAL AX1                     ! X NDC coord at left of AGI picture
      REAL AX2                     ! X NDC coord at right of AGI picture
      REAL AXL                     ! X world coord at left of AGI picture
      REAL AXR                     ! X world coord at right of AGI picture
      REAL AY1                     ! Y NDC coord at bottom of AGI picture
      REAL AY2                     ! Y NDC coord at top of AGI picture
      REAL AYB                     ! Y world coord at bottom of AGI picture
      REAL AYT                     ! Y world coord at top of AGI picture
      REAL PXL                     ! X world coord at left of original window
      REAL PXR                     ! X world coord at right of original window
      REAL PYB                     ! Y world coord at bottom of original window
      REAL PYT                     ! Y world coord at top of original window
      REAL X1                      ! X NDC coord at left of original viewport
      REAL X2                      ! X NDC coord at right of original viewport
      REAL Y1                      ! Y NDC coord at bottom of original viewport
      REAL Y2                      ! Y NDC coord at top of original viewport
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin as AST context.
      CALL AST_BEGIN( STATUS )

*  If an AGI picture ID has been supplied, we extend the pgplot viewport
*  to cover the picture.
      IF( IPIC .NE. -1 ) THEN

*  Get the size of the current PGPLOT window.
         CALL PGQWIN( PXL, PXR, PYB, PYT )

*  Get the size of the current PGPLOT viewport, in NDC.
         CALL PGQVP( 0, X1, X2, Y1, Y2)

*  Save the index of the current AGI picture, and make the supplied
*  picture current.
         CALL AGI_ICURP( IPIC0, STATUS )
         CALL AGI_SELP( IPIC, STATUS )

*  Set the pgplot viewport and window so that they correspond to the 
*  supplied picture.
         CALL AGP_NVIEW( .FALSE., STATUS )

*  Re-instate the original current picture.
         CALL AGI_SELP( IPIC0, STATUS )

*  Get the viewport corresponding to the supplied picture, in NDC.
         CALL PGQVP( 0, AX1, AX2, AY1, AY2 )

*  Determine the size of the PGPLOT window covering the supplied picture.
         IF( X2 .NE. X1 .AND. Y2 .NE. Y1 .AND. 
     :       STATUS .EQ. SAI__OK ) THEN

            A = ( PXR - PXL )/( X2 - X1 )
            AXL = A*( AX1 -X1 ) + PXL
            AXR = A*( AX2 -X1 ) + PXL

            A = ( PYT - PYB )/( Y2 - Y1 )
            AYB = A*( AY1 -Y1 ) + PYB
            AYT = A*( AY2 -Y1 ) + PYB

*  Set the extended PGPLOT window.
            CALL PGSWIN( AXL, AXR, AYB, AYT )

         END IF

      END IF

*  Simplify the Plot. This adds a new Current Frame into the Plot, so
*  take a deep copy of it first. This can help to speed up the drawing, 
*  and also avoids the possibility of the Mapping going via a Frame in 
*  which the positions are undefined.
      IPLOT2 = AST_COPY( IPLOT, STATUS )
      CALL KPG1_ASSIM( IPLOT2, STATUS )

*  Draw the grid or border within a PGPLOT buffering context.
      IF( STATUS .EQ. SAI__OK ) THEN 
         CALL PGBBUF

*  See if the current Frame of the Plot contains a DSBSpecFrame, noting the 
*  names of the relevant attributes is a DSBSpecFrame is found. If the
*  horizontal axis is represented by a DSBSpecFrame, then the title gap
*  needs to be increased in order to make room for the axis annotation on
*  the upper edge.
         TEXT = ' '
         IAT = 0
         CALL CHR_APPND( 'Grid=0,Tickall=0,DrawTitle=0,DrawAxes=0,', 
     :                   TEXT, IAT )

         AX = AST_PICKAXES( IPLOT2, 1, 1, MAP, STATUS )
         IF( KPG1_GETASTDSB() .AND. 
     :       AST_ISADSBSPECFRAME( AX, STATUS ) ) THEN
            AEDGE = 'Edge(1)'
            ASB = 'SideBand(1)'
            AGAP = 'TextLabGap(1)'
            CALL CHR_APPND( 'NumLab(2)=0,TextLab(2)=0', TEXT, IAT )
         
            VGAP = AST_GETD( IPLOT2, AGAP, status )
            CALL AST_SETD( IPLOT2, 'TitleGap', 2.5*VGAP, STATUS )
            CALL AST_SET( IPLOT2, 'TickAll=0', STATUS )

         ELSE
            AX = AST_PICKAXES( IPLOT2, 1, 2, MAP, STATUS )
            IF( KPG1_GETASTDSB() .AND. 
     :          AST_ISADSBSPECFRAME( AX, STATUS ) ) THEN
               AEDGE = 'Edge(2)'
               ASB = 'SideBand(2)'
               AGAP = 'TextLabGap(2)'
               CALL CHR_APPND( 'NumLab(1)=0,TextLab(1)=0', TEXT, IAT)
   
               VGAP = AST_GETD( IPLOT2, AGAP, status )
               CALL AST_SETD( IPLOT2, 'TitleGap', 2.5*VGAP, STATUS )
               CALL AST_SET( IPLOT2, 'TickAll=0', STATUS )

            ELSE
               AEDGE = ' '
               ASB = ' '
            END IF
         END IF

*  Draw the first grid.
         IF( GRID ) THEN
            CALL AST_GRID( IPLOT2, STATUS )

*  If a DSBSpecFrame was found, and the labelling is exterior, we will
*  draw a second grid annotating the other side band. Take a copy 
*  of the plot so that we do not change it.
            IF( AEDGE .NE. ' ' .AND. AST_GETC( IPLOT2, 'Labelling', 
     :                                         STATUS ) 
     :                               .EQ. 'exterior' ) THEN

*  Switch off drawing off everything to do with the other axis.
               CALL AST_SET( IPLOT2, TEXT( : IAT ), STATUS )

*  Make room for the title above the upper edge label
               CALL AST_SETD( IPLOT2, AGAP, 0.5*VGAP, STATUS )

*  Indicate that the opposite edge should be annotated
               VEDGE = AST_GETC( IPLOT2, AEDGE, STATUS )
               CALL CHR_LCASE( VEDGE )
               IF( VEDGE .EQ. 'left' ) THEN
                  VEDGE = 'right'

               ELSE IF( VEDGE .EQ. 'right' ) THEN
                  VEDGE = 'left'

               ELSE IF( VEDGE .EQ. 'top' ) THEN
                  VEDGE = 'bottom'

               ELSE 
                  VEDGE = 'top'
               END IF

               CALL AST_SETC( IPLOT2, AEDGE, VEDGE, STATUS )

*  Indicate that values for the opposite side band should be annotated
               VSB = AST_GETC( IPLOT2, ASB, STATUS )
               CALL CHR_LCASE( VSB )
               IF( VSB .EQ. 'usb' ) THEN
                  VSB = 'lsb'
               ELSE 
                  VSB = 'usb'
               END IF

               CALL AST_SETC( IPLOT2, ASB, VSB, STATUS )

*  Draw a second grid over the first.
               CALL AST_GRID( IPLOT2, STATUS )

            END IF

*  Draw a border instead of a grid if requested.
         ELSE
            BOX = AST_BORDER( IPLOT2, STATUS )
         END IF

         IF( STATUS .EQ. AST__VSMAL ) THEN
            CALL ERR_REP( 'KPG1_ASGRD_ERR1', 'This problem may '//
     :         'possibly be overcome by using WCSFRAME to change '//
     :         'the current WCS co-ordinate Frame in the data, or by '//
     :         ' changing the program parameter values being used.', 
     :         STATUS )
         END IF 
         CALL PGEBUF
      END IF

*  Re-instate the original PGPLOT viewport and window if necessary.
      IF( IPIC .NE. -1 .AND. STATUS .EQ. SAI__OK ) THEN
         CALL PGSVP( X1, X2, Y1, Y2 )
         CALL PGSWIN( PXL, PXR, PYB, PYT )
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
