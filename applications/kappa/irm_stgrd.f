      SUBROUTINE IRM_STGRD( X1, X2, Y1, Y2, STATUS )
*+
*  Name:
*     IRM_STGRD

*  Purpose:
*     Set the position and the size of grid window of NCAR package

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_STGRD( X1, X2, Y1, Y2, STATUS )

*  Description:
*     This routine set the NCAR parameters 'GRID/LEFT.', 'GID/RIGHT',
*     'GRID/BOTTOM.' and 'GRID/TOP.' according to the given
*     specifications to specifies the position and the shape of the grid
*     window within the graph window.  

*  Arguments:
*     X1, X2 = REAL (Given)
*        The X extent of the NCAR grid window, given in fraction of the 
*        graph-window (current SGS zone ) width.
*     Y1, Y2 = REAL (Given)
*        The Y extent of the NCAR grid window, given in fraction of the 
*        graph-window (current SGS zone ) hight.  
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     11-JAN-1991 (WG):
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
      REAL X1, X2, Y1, Y2

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the NCAR shape parameter for grid window so that the shape of the
*  grid window is exactly the same shape as the specified by the user.
      CALL AGSETF( 'GRID/SHAP.', 0. )

*  Set the NCAR grid window position parameters
      CALL AGSETF( 'GRID/LEFT.', X1 )
      CALL AGSETF( 'GRID/RIGHT.', X2 )
      CALL AGSETF( 'GRID/BOTTOM.', Y1 )
      CALL AGSETF( 'GRID/TOP.', Y2 )

 999  CONTINUE
      END
