      SUBROUTINE KPG1_PRNTH( STRING, OP, CL, STATUS )
*+
*  Name:
*     KPG1_PRNTH

*  Purpose:
*     Locates the outer-most pair of parenthesis in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PRNTH( STRING, OP, CL STATUS )

*  Description:
*     The routine returns the indices of the first opening parenthesis and 
*     the last closing parenthesis in the supplied string. Both are
*     returned equal to zero if either parenthesis is not found.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        String to be searched.
*     OP = INTEGER (Returned)
*        Position within STRING at which the first occurence of "(" is
*        located.
*     CL = INTEGER (Returned)
*        Position within STRING at which the last occurence of ")" is
*        located.
*     STATUS = INTEGER (Given and Returned)
*        The global status.   

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-MAR-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER STRING*(*)

*  Arguments Returned:
      INTEGER OP
      INTEGER CL

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Initialise.
      OP = 0
      CL = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first opening parenthesis.
      OP = INDEX( STRING, '(' )

* Only continue if it was found.
      IF( OP .GT. 0 ) THEN

*  Find the last closing parenthesis in the string following the
*  opening parenthesis.
         CL = 0
         CALL KPG1_LASTO( STRING( OP + 1 : ), ')', CL, STATUS )


*  If not found, return zero for the index of the opening parenthesis.
         IF( CL .EQ. 0 ) THEN
            OP = 0

*  Otherwise, correct the returned index to take account of the starting
*  index in the call to KPG1_LASTO.
         ELSE
            CL = CL + OP
         END IF

      END IF

      END
