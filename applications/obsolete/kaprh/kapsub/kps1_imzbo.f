      SUBROUTINE KPS1_IMZBO( BOUNDS, ZONE, STATUS )
*+
*  Name:
*     KPS1_IMZBO

*  Purpose:
*     Creates a smaller image zone after making allowances for a border.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_IMZBO( BOUNDS, ZONE, STATUS )

*  Description:
*     This routine takes fractional bounds in an SGS zone to define a
*     new SGS zone preserving the world-coordinate system.

*  Arguments:
*     BOUNDS( 4 ) = REAL (Given)
*        The fractional bounds in the unit zone where the new image
*        zone is to be located.  The order is x_lower, x_upper, y_lower,
*        and y_upper.  These are not validated as this is called by
*        display applications that have these in parameter statements.
*     ZONE = INTEGER (Returned)
*        The new image zone after clipping.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  SGS must be active.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 March 5 (MJC):
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
      REAL
     :  BOUNDS( 4 )

*  Arguments Returned:
      INTEGER
     :  ZONE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL
     :  T1, T2,                  ! Dummy variables for rescaling image
     :  X1, X2, Y1, Y2,          ! Zone size in world co-ordinates
     :  XM, YM                   ! Zone size in metres (not used)

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Obtain the bounds of the current zone.

      CALL SGS_IZONE( X1, X2, Y1, Y2, XM, YM )

*    Redefine the bounds of the image zone in terms of device pixels,
*    otherwise the centering and scaling will be wrong.  Dummy variables
*    must be used as the limits are changing during the calculations.

      T1 = X1
      T2 = X2
      X1 = T1 + BOUNDS( 1 ) * ( T2 - T1 )
      X2 = T1 + BOUNDS( 2 ) * ( T2 - T1 )

      T1 = Y1
      T2 = Y2
      Y1 = T1 + BOUNDS( 3 ) * ( T2 - T1 )
      Y2 = T1 + BOUNDS( 4 ) * ( T2 - T1 )

*    This defines the new "initial" image zone and the world
*    co-ordinates.

      CALL SGS_ZONE( X1, X2, Y1, Y2, ZONE, STATUS )
      CALL SGS_SW( X1, X2, Y1, Y2, STATUS )

      END
