      SUBROUTINE UDI0_TYPSIZ( TYPE, SIZE, STATUS )
*+
*  Name:
*     UDI0_TYPSIZ

*  Purpose:
*     Return size of a type in bytes

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UDI0_TYPSIZ( TYPE, SIZE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     TYPE = CHARACTER*(*) (given)
*        The type whose size is required
*     SIZE = INTEGER (returned)
*        The size of the type in bytes
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
*     UDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/udi.html

*  Keywords:
*     package:udi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     30 Aug 1995 (DJA):
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
      CHARACTER*(*)		TYPE

*  Arguments Returned:
      INTEGER			SIZE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*7		LTYPE			! Local copy of TYPE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Trap HDS style types
      FC = 1
      IF ( TYPE(1:1) .EQ. '_' ) FC = 2

*  Make local copy and capitalise
      LTYPE = TYPE(FC:)
      CALL CHR_UCASE( LTYPE )

*  Test against alternatives
      IF ( LTYPE .EQ. 'REAL' ) THEN
        SIZE = VAL__NBR
      ELSE IF ( LTYPE .EQ. 'INTEGER' ) THEN
        SIZE = VAL__NBI
      ELSE IF ( LTYPE .EQ. 'LOGICAL' ) THEN
        SIZE = VAL__NBI
      ELSE IF ( LTYPE .EQ. 'DOUBLE' ) THEN
        SIZE = VAL__NBD
      ELSE IF ( ((LTYPE .EQ. 'BYTE') .OR. (LTYPE .EQ. 'UBYTE') ) THEN
        SIZE = VAL__NBB
      ELSE IF ( ((LTYPE .EQ. 'WORD') .OR. (LTYPE .EQ. 'UWORD') ) THEN
        SIZE = VAL__NBW
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( ' ', 'Unrecognised type string ^T', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'UDI0_TYPSIZ', STATUS )

      END
