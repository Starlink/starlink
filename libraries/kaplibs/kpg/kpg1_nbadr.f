      SUBROUTINE KPG1_NBADR( N, DATA, NBAD, STATUS )
*+
*  Name:
*     KPG1_NBADR
 
*  Purpose:
*     Finds the number of bad values in an array.
 
*  Description:
*     Finds the number of bad values in a 1-D array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation
*     CALL KPG1_NBADR( N, DATA, NBAD, STATUS )
 
*  Arguments:
*     N = INTEGER (Given)
*        Number of elements in the array.
*     DATA( N ) = ? (Given)
*        The data array.
*     NBAD = INTEGER (Returned)
*        The number of bad values in the data.
*     STATUS = INTEGER (Given)
*        The global status.
 
*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     9-DEC-1998 (DSB):
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
      REAL DATA( N )
 
*  Arguments Returned:
      INTEGER NBAD
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop count
 
*.
 
*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Initialise the returned count.
      NBAD = 0
 
*  Loop round the data array, counting bad values.
      DO I = 1, N
         IF ( DATA( I ) .EQ. VAL__BADR ) NBAD = NBAD + 1
      END DO
 
      END
