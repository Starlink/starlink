      SUBROUTINE POL1_SQUAR( N, DATA, STATUS )
*+
*  Name:
*     POL1_SQUAR

*  Purpose:
*     Square the supplied array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SQUAR( N, DATA, STATUS )

*  Description:
*     This routine squares the values in the supplied array, storing them
*     back in the same array. Checks are made for VAL__BADR values.

*  Arguments:
*     N = INTEGER (Given)
*        The number of points in DATA.
*     DATA( N ) = REAL (Given and Returned)
*        The data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-MAR-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER N

*  Arguments Given and Returned:
      REAL DATA( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! loop index
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, N
         IF( DATA( I ) .NE. VAL__BADR ) THEN
            DATA( I ) = DATA( I )**2
         END IF
      END DO

      END
