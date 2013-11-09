      SUBROUTINE BDI0_INIT<T>( INIT, N, DATA, STATUS )
*+
*  Name:
*     BDI0_INIT<T>

*  Purpose:
*     Initialise an array with a symbolic value

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_INIT<T>( INIT, N, DATA, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     INIT = CHARACTER*(*) (given)
*        The symbolic initialiser. May be BAD or ZERO at the moment
*     N = INTEGER (given)
*        Number of points to initialise
*     DATA[*] = <TYPE> (returned)
*        The initialised data
*     STATUS = INTEGER (given and returned)
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     13 Mar 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      CHARACTER*(*)		INIT
      INTEGER			N

*  Arguments Returned:
      <TYPE>			DATA(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      <TYPE>			INV			! Initialisation value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Select value
      IF ( INIT .EQ. 'ZERO' ) THEN
        INV = 0
      ELSE IF ( INIT .EQ. 'BAD' ) THEN
        INV = VAL__BAD<T>
      ELSE
        CALL MSG_SETC( 'I', INIT )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Inrecognised <COMM> initialiser ^I',
     :                STATUS )
      END IF

*  Initialise
      CALL ARR_INIT1<T>( INV, N, DATA, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI0_INIT<T>', STATUS )

      END
