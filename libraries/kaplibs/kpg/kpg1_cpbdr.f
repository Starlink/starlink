      SUBROUTINE KPG1_CPBDR( N, D1, D2, OUT, STATUS )
*+
*  Name:
*     KPG1_CPBDR

*  Purpose:
*     Copy bad pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CPBDR( N, D1, D2, OUT, STATUS )

*  Description:
*     This routine copies the bad pixels from one array into another.

*  Arguments:
*     N = INTEGER (Given)
*        No. of points
*     D1( N ) = REAL (Given)
*        The original data values.
*     D2( N ) = REAL (Given)
*        An associated mask array. 
*     OUT( N ) = REAL (Given)
*        The returned array. Equal to D1 if both D1 and D2 are good (bad
*        if either D1 or D2 is bad).
*     STATUS = INTEGER (Given and Returned) 
*        The global status.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1999 (DSB):
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
      REAL D1( N )
      REAL D2( N )

*  Arguments Returned:
      REAL OUT( N )

*  Status:
      INTEGER STATUS          ! Global status

*  Local Variables:
      INTEGER I               ! Loop count
*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Just do it.
      DO I = 1, N
         IF( D2( I ) .NE. VAL__BADR ) THEN
            OUT( I ) = D1( I )
         ELSE
            OUT( I ) = VAL__BADR
         END IF
      END DO

      END
