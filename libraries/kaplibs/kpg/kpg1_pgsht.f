      SUBROUTINE KPG1_PGSHT( HGT, STATUS )
*+
*  Name:
*     KPG1_PGSHT

*  Purpose:
*     Set the PGPLOT character size in world coordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL  KPG1_PGSHT( HGT, STATUS )

*  Description:
*     This routine sets the PGPLOT character size to a specified value in
*     world coordinates. It mimics SGS_SHTX in so far as this is possible.
*
*     Note SGS and PGPLOT behave differently if the scales on the X and Y
*     axes are not equal. SGS keeps the character size constant in world
*     oordinates, so absolute character size will be different for vertical
*     and horizontal text. On the other hand, PGPLOT keeps the absolute
*     character size fixed, resulting in the characters size in world
*     coordinates varying for horizontal and vertical text. This routine
*     sets the size for horizontal text. If the axis scales are not equal, 
*     vertical text will have have a different size (in world coordinates).

*  Arguments:
*     HGT = REAL (Given)
*        The required character height, in world coordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-MAR-1998 (DSB):
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
      REAL HGT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL FACTOR                ! Dimensionless scale factor for text size 
      REAL XCH                   ! Text size in X axis units
      REAL YCH                   ! Text size in Y axis units
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the current PGPLOT character heights in world coordinates.
      CALL PGQCS( 4, XCH, YCH )

*  Get the current scale factor for character size.
      CALL PGQCH( FACTOR )

*  Set the new scale factor to give the required height in world
*  coordinates (assuming horizontal text).
      IF( YCH .NE. 0.0 ) CALL PGSCH( HGT*FACTOR/YCH )

      END
