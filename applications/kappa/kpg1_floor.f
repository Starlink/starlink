      INTEGER FUNCTION KPG1_FLOOR( VALUE )
*+
*  Name:
*     KPG1_FLOOR

*  Purpose:
*     Return the largest integer smaller than or equal to a supplied value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_FLOOR( VALUE )

*  Description:
*     This routine returns the largest integer smaller than or equal to
*     a supplied value.

*  Arguments:
*     VALUE = REAL (Given)
*        The value.

*  Function Value:
*     KPG1_CEIL = INTEGER
*        The largest integer smaller than or equal to the supplied value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      REAL VALUE

*  Do it.
      IF( VALUE > VAL__MAXI ) THEN
         KPG1_FLOOR = VAL__MAXI

      ELSE IF( VALUE < VAL__MINI ) THEN
         KPG1_FLOOR = VAL__MINI

      ELSE
         KPG1_FLOOR = INT( VALUE )
         IF( REAL( KPG1_FLOOR ) .GT. VALUE ) KPG1_FLOOR = KPG1_FLOOR 
     :                                                    - 1.0
      END IF

      END
 
