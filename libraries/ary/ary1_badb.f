      SUBROUTINE ARY1_BADB( N, ARGV, STATUS )
*+
*  Name:
*     ARY1_BADB
 
*  Purpose:
*     Set all elements of a BYTE vectorised array to VAL__BADB.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ARY1_BADB( N, ARGV, STATUS )
 
*  Description:
*     The routine sets each element of the BYTE vectorised array
*     supplied to the "bad" value specified by the global constant
*     VAL__BADB.
 
*  Arguments:
*     N = INTEGER (Given)
*        Number of array elements.
*     ARGV( N ) = BYTE (Returned)
*        The BYTE vectorised array whose elements are to be set.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Algorithm:
*     -  Set each element to VAL__BADB with an assignment statement.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     8-JUN-1989  (RFWS):
*        Original version.
*     13-MAR-1990 (RFWS):
*        Renamed from VEC_BADB to ARY1_BADB.
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
      BYTE ARGV( N )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local variables:
      INTEGER I                  ! Loop counter for array elements
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Assign the value VAL__BADB to each array element in turn.
      DO 1 I = 1, N
         ARGV( I ) = VAL__BADB
1     CONTINUE
 
      END
