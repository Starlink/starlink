      SUBROUTINE ARY1_BADI( N, ARGV, STATUS )
*+
*  Name:
*     ARY1_BADI
 
*  Purpose:
*     Set all elements of a INTEGER vectorised array to VAL__BADI.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ARY1_BADI( N, ARGV, STATUS )
 
*  Description:
*     The routine sets each element of the INTEGER vectorised array
*     supplied to the "bad" value specified by the global constant
*     VAL__BADI.
 
*  Arguments:
*     N = INTEGER (Given)
*        Number of array elements.
*     ARGV( N ) = INTEGER (Returned)
*        The INTEGER vectorised array whose elements are to be set.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Algorithm:
*     -  Set each element to VAL__BADI with an assignment statement.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     8-JUN-1989  (RFWS):
*        Original version.
*     13-MAR-1990 (RFWS):
*        Renamed from VEC_BADI to ARY1_BADI.
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
      INTEGER ARGV( N )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local variables:
      INTEGER I                  ! Loop counter for array elements
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Assign the value VAL__BADI to each array element in turn.
      DO 1 I = 1, N
         ARGV( I ) = VAL__BADI
1     CONTINUE
 
      END
