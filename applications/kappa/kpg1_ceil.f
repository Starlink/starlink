      INTEGER FUNCTION KPG1_CEIL( VALUE )
*+
*  Name:
*     KPG1_CEIL

*  Purpose:
*     Return the smallest integer larger than or equal to a supplied value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_CEIL( VALUE )

*  Description:
*     This routine returns the smallest integer larger than or equal to
*     a supplied value.

*  Arguments:
*     VALUE = REAL (Given)
*        The value.

*  Function Value:
*     KPG1_CEIL = INTEGER
*        The smallest integer larger than or equal to the supplied value.

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
         KPG1_CEIL = VAL__MAXI

      ELSE IF( VALUE < VAL__MINI ) THEN
         KPG1_CEIL = VAL__MINI

      ELSE
         KPG1_CEIL = INT( VALUE )
         IF( REAL( KPG1_CEIL ) .LT. VALUE ) KPG1_CEIL = KPG1_CEIL + 1.0
      END IF

      END
 
