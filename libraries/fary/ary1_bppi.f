      SUBROUTINE ARY1_BPPI( EL, ARRAY, BAD, STATUS )
*+
*  Name:
*     ARY1_BPPI
 
*  Purpose:
*     Determine if bad pixels are present in a vectorised INTEGER array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ARY1_BPPI( EL, ARRAY, BAD, STATUS )
 
*  Description:
*     The routine examines the values in a vectorised INTEGER array and
*     returns a logical result BAD indicating whether any element of the
*     array contains the "bad" pixel value VAL__BADI.
 
*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in the vectorised array.
*     ARRAY( EL ) = INTEGER (Given)
*        The array to be examined.
*     BAD = LOGICAL (Returned)
*        Whether any ARRAY element had the value VAL__BADI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Algorithm:
*     -  Initialise.
*     -  Loop to examine each array element.
*     -  If a bad value is found, then set BAD=.TRUE. and quit checking.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     21-NOV-1989 (RFWS):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
 
*  Arguments Given:
      INTEGER EL
      INTEGER ARRAY( EL )
 
*  Arguments Returned:
      LOGICAL BAD
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Initialise.
      BAD = .FALSE.
 
*  Loop to examine each array element.
      DO 1 I = 1, EL
 
*  If a bad value is found, set BAD=.TRUE. and quit checking.
         IF ( ARRAY( I ) .EQ. VAL__BADI ) THEN
            BAD = .TRUE.
            GO TO 2
         END IF
1     CONTINUE
2     CONTINUE
 
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_BPPI',
     :                                            STATUS )
 
      END
