      SUBROUTINE ARY1_BADR( N, ARGV, STATUS )
*+
*  Name:
*     ARY1_BADR
 
*  Purpose:
*     Set all elements of a REAL vectorised array to VAL__BADR.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ARY1_BADR( N, ARGV, STATUS )
 
*  Description:
*     The routine sets each element of the REAL vectorised array
*     supplied to the "bad" value specified by the global constant
*     VAL__BADR.
 
*  Arguments:
*     N = INTEGER (Given)
*        Number of array elements.
*     ARGV( N ) = REAL (Returned)
*        The REAL vectorised array whose elements are to be set.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Algorithm:
*     -  Set each element to VAL__BADR with an assignment statement.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     8-JUN-1989  (RFWS):
*        Original version.
*     13-MAR-1990 (RFWS):
*        Renamed from VEC_BADR to ARY1_BADR.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
 
*  Arguments Given:
      INTEGER N
 
*  Arguments Returned:
      REAL ARGV( N )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local variables:
      INTEGER I                  ! Loop counter for array elements
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Assign the value VAL__BADR to each array element in turn.
      DO 1 I = 1, N
         ARGV( I ) = VAL__BADR
1     CONTINUE
 
      END
