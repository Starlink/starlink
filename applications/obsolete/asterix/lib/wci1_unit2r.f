      SUBROUTINE WCI1_UNIT2R( UNIT, FACTOR, STATUS )
*+
*  Name:
*     WCI1_UNIT2R

*  Purpose:
*     Return conversion factor from units supplied to radians

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WCI1_UNIT2R( UNIT, FACTOR, STATUS )

*  Description:
*     Returns conversion factor from angular units UNIT to radians. An
*     error is reported if the units cannot be recognised.

*  Arguments:
*     UNIT = CHARACTER*(*) (given)
*        The units to convert
*     FACTOR = DOUBLE (returned)
*        The quantity such that a number with units UNIT can be converted
*        to radians by multiplication
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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! Standard SAE
      INCLUDE 'MATH_PAR'				! ASTERIX maths

*  Arguments Given:
      CHARACTER*(*)		UNIT			! Units string

*  Arguments Returned:
      DOUBLE PRECISION		FACTOR			! Conversion factor

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			STR_ABBREV
        LOGICAL			STR_ABBREV
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Always return a default
      FACTOR = 1D0

      IF ( STR_ABBREV( UNIT, 'DEGREES' ) ) THEN
        FACTOR = MATH__DDTOR

      ELSE IF ( STR_ABBREV( UNIT, 'ARCMINUTES' ) .OR.
     :                          (UNIT.EQ.'ARCMINS') ) THEN
        FACTOR = MATH__DDTOR / 60D0

      ELSE IF ( STR_ABBREV( UNIT, 'ARCSECONDS' ) .OR.
     :                          (UNIT.EQ.'ARCSECS') ) THEN
        FACTOR = MATH__DDTOR / 3600D0

      ELSE IF ( STR_ABBREV(UNIT, 'RADIANS') ) THEN
        FACTOR = 1D0

      ELSE
        CALL MSG_SETC( 'UN', UNIT )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unrecognised angular units ^UN', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI1_UNIT2R', STATUS )

      END
