      SUBROUTINE KPG1_GDWIN( IPIC, STATUS )
*+
*  Name:
*     KPG1_GDWIN

*  Purpose:
*     Set PGPLOT world co-ordinates to be the world co-ordinates 
*     of the specified AGI picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  KPG1_GDWIN( IPIC, STATUS )

*  Description:
*     This routine finds the bounds of the current PGPLOT viewport within 
*     the world-coordinate system of a specified AGI picture, and sets the
*     PGPLOT window accordingly.

*  Arguments:
*     IPIC = INTEGER (Given)
*        The AGI picture identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-AUG-1998 (DSB):
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
      INTEGER IPIC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPIC0              ! Original AGI current picture identifier
      REAL M                     ! Ratio of window and viewport scales
      REAL NWX1, NWX2, NWY1, NWY2! New window bounds for original viewport
      REAL PX1, PX2, PY1, PY2    ! Viewport bounds (NDC) of specified picture
      REAL VX1, VX2, VY1, VY2    ! Original viewport bounds (NDC) 
      REAL WX1, WX2, WY1, WY2    ! Window bounds of specified picture
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the NDC bounds of the current PGPLOT viewport.
      CALL PGQVP( 0, VX1, VX2, VY1, VY2 )
      
*  Save the current AGI picture.
      CALL AGI_ICURP( IPIC0, STATUS )

*  Make the specified AGI picture current.
      CALL AGI_SELP( IPIC, STATUS )

*  Set the PGPLOT viewport and window so that they correspond to the
*  specified picture.
      CALL AGP_NVIEW( .FALSE., STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounds of the specified picture's viewport in NDC.
      CALL PGQVP( 0, PX1, PX2, PY1, PY2 )

*  Only proceed if the viewport has some area.
      IF( PX1 .NE. PX2 .AND. PY1 .NE. PY2 ) THEN

*  Get the bounds of the specified picture's window in world co-ordinates.
         CALL PGQWIN( WX1, WX2, WY1, WY2 )

*  Get the increment in world co-ord for unit increment in NDC
*  on the X axis.
         M = ( WX1 - WX2 ) / ( PX1 - PX2 )

*  Find the X axis world co-ord bounds corresponding to the original
*  viewport bounds.
         NWX1 = M*( VX1 - PX1 ) + WX1
         NWX2 = M*( VX2 - PX1 ) + WX1

*  Get the increment in world co-ord for unit increment in NDC
*  on the Y axis.
         M = ( WY1 - WY2 ) / ( PY1 - PY2 )

*  Find the Y axis world co-ord bounds corresponding to the original
*  viewport bounds.
         NWY1 = M*( VY1 - PY1 ) + WY1
         NWY2 = M*( VY2 - PY1 ) + WY1

*  Set the PGPLOT window to these bounds.
         CALL PGSWIN( NWX1, NWX2, NWY1, NWY2 )

      END IF

*  Make the original AGI picture current.
      CALL AGI_SELP( IPIC0, STATUS )

*  Re-establish the original viewport.
      CALL PGSVP( VX1, VX2, VY1, VY2 )

 999  CONTINUE

      END
