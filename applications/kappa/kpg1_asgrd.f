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
      INTEGER IPLOT
      INTEGER IPIC
      LOGICAL GRID

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      INTEGER ICURR                ! Index of original current frame
      INTEGER IPIC0                ! AGI id for original current picture
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

*  Simplify the Plot. This adds a new Current Frame into the Plot, so note 
*  the index of the original Current Frame so that it can be re-instated later.
*  This can help to speed up the drawing, and also avoids the possibility
*  of the Mapping going via a Frame in which the positions are undefined.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )
      CALL KPG1_ASSIM( IPLOT, STATUS )

*  Draw the grid or border within a PGPLOT buffering context.
      CALL PGBBUF

      IF( GRID ) THEN
         CALL AST_GRID( IPLOT, STATUS )
      ELSE
         BOX = AST_BORDER( IPLOT, STATUS )
      END IF

      CALL PGEBUF

*  Remove the Current Frame added by KPG1_ASSIM and re-instate the original 
*  Current Frame.
      CALL AST_REMOVEFRAME( IPLOT, AST__CURRENT, STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

*  Re-instate the original PGPLOT viewport and window if necessary.
      IF( IPIC .NE. -1 .AND. STATUS .EQ. SAI__OK ) THEN
         CALL PGSVP( X1, X2, Y1, Y2 )
         CALL PGSWIN( PXL, PXR, PYB, PYT )
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
