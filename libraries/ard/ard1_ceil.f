      INTEGER FUNCTION ARD1_CEIL( VALUE )
*+
*  Name:
*     ARD1_CEIL

*  Purpose:
*     Return the smallest integer larger than or equal to a supplied value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = ARD1_CEIL( VALUE )

*  Description:
*     This routine returns the smallest integer larger than or equal to
*     a supplied value.

*  Arguments:
*     VALUE = DOUBLE PRECISION (Given)
*        The value.

*  Function Value:
*     ARD1_CEIL = INTEGER
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
      DOUBLE PRECISION VALUE

*  Do it.
      IF( VALUE .GT. VAL__MAXI ) THEN
         ARD1_CEIL = VAL__MAXI

      ELSE IF( VALUE .LT. VAL__MINI ) THEN
         ARD1_CEIL = VAL__MINI

      ELSE
         ARD1_CEIL = INT( VALUE )
         IF( DBLE( ARD1_CEIL ) .LT. VALUE ) ARD1_CEIL = ARD1_CEIL + 1
      END IF

      END
 
