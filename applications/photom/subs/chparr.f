************************************************************************

      SUBROUTINE CHPARR ( NAME, VALUE, LOWER, UPPER, STATUS )

*+
*  Name :
*     CHPARR
*
*  Purpose :
*     Check the limits of a real scalar parameter
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL CHPARR( NAME, VALUE, LOWER, UPPER, STATUS )
*
*  Description :
*     This takes a real scalar parameter and checks that the value lies
*     between the lower and upper limits. If this is not the case then
*     it keeps prompting for a new value until this is the case.
*
*  Arguments :
*     NAME =  CHARACTER*(*) (Given)
*        Name of parameter as in interface file
*     VALUE = REAL (Given and Returned)
*        Value of parameter
*     LOWER = REAL (Given)
*        Lower limit for range check
*     UPPER = REAL (Given)
*        Upper limit for range check
*     INTEGER STATUS (Given and Returned)
*        The global status.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-JUL-1988 (NE):
*        Original version.
*      3-MAR-1992 (NE):
*        Replace call to ERR_OUT with MSG_OUT
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'


*  Arguments Given :
      CHARACTER * ( * ) NAME
      REAL LOWER
      REAL UPPER

*  Arguments Given and Returned :
      REAL VALUE

*  Status :
      INTEGER STATUS
*.

      IF ( STATUS .EQ. SAI__OK ) THEN

*   See if value is outside the specified range
         DO WHILE ( ( VALUE .LT. LOWER ) .OR. ( VALUE .GT. UPPER ) )

*   Report an error
            CALL MSG_OUT( 'PAR_ERR', 'Invalid parameter value', STATUS )

*   Cancel the parameter and try again
            CALL PAR_CANCL( NAME, STATUS )
            CALL PAR_GET0R( NAME, VALUE, STATUS )

         ENDDO

      ENDIF

      END

* $Id$
