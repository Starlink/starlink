      SUBROUTINE AST_REXIT( RTN, STATUS )
*+
*  Name:
*     AST_REXIT

*  Purpose:
*     Output a error message from a routine

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AST_REXIT( RTN, STATUS )

*  Description:
*     Reports a standard error given a routine name so that error dumps
*     have a uniform neat appearance. Parameter system aborts are ignored.

*  Arguments:
*     RTN = CHARACTER*(*) (given)
*        The name of the routine reporting the error
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     AST Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/ast.html

*  Keywords:
*     package:ast, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Apr 1989 (DJA):
*        Original version.
*     21 Feb 1996 (DJA):
*        Removed concatenation of input argument to build on Linux
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'

*  Arguments Given:
      CHARACTER*(*)		RTN

*  Status:
      INTEGER			STATUS             	! Global status
*.

*    Check status is not a parameter abort
      IF ( (STATUS.NE.PAR__NULL) .OR. (STATUS.NE.PAR__ABORT) ) THEN

*      Tag the routine name on the end
        CALL MSG_SETC( 'R', RTN )
        CALL ERR_REP( ' ', '...from ^R', STATUS )

      END IF

      END
