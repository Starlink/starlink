      SUBROUTINE ARY1_ZERR( N, ARGV, STATUS )
*+
*  Name:
*     ARY1_ZERR
 
*  Purpose:
*     Set all elements of a vectorized REAL array to zero.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ARY1_ZERR( N, ARGV, STATUS )
 
*  Description:
*     The routine sets all the elements of the REAL array supplied to
*     zero.
 
*  Arguments:
*     N = INTEGER (Given)
*        Number of elements in the array.
*     ARGV( N ) = REAL (Returned)
*        The vectorised REAL array to be set to zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Algorithm:
*     -  Set each array element to zero with an assignment statement.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     8-JUN-1989  (RFWS):
*        Original version.
*     30-AUG-1989 (RFWS):
*        Changed name to conform to prefix plus 5 character convention.
*     13-MAR-1990 (RFWS):
*        Renamed from VEC_ZERR to ARY1_ZERR.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
 
*  Arguments Given:
      INTEGER N
 
*  Arguments Returned:
      REAL ARGV( N )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Constants:
      REAL ZERO                ! Zero
      PARAMETER ( ZERO = 0.0E0 )
 
*  Local variables:
      INTEGER I                  ! Loop counter for array elements
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Assign the value ZERO to each array element.
      DO 1 I = 1, N
         ARGV( I ) = ZERO
1     CONTINUE
 
      END
