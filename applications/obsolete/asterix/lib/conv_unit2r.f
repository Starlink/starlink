      SUBROUTINE CONV_UNIT2R( UNITS, CONV, STATUS )
*+
*  Name:
*     CONV_UNIT2R

*  Purpose:
*     Convert angular unit string to radian conversion factor

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL CONV_UNIT2R( UNITS, CONV, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     UNITS = CHARACTER*(*) (given)
*        The units string
*     CONV = REAL (returned)
*        Conversion factor from UNITS to radians
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
*     Should really be a double precision factor returned

*  References:
*     CONV Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/conv.html

*  Keywords:
*     package:conv, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      7 Sep 1989 (DJA):
*        Original version.
*     19 Feb 1996 (DJA):
*        No direct terminal output
*     20 Feb 1997 (RB):
*        Add case for pixels (meaningless)
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MATH_PAR'

*  Arguments Given:
      CHARACTER*(*)		UNITS

*  Arguments Returned:
      REAL			CONV

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      LOGICAL			STR_ABBREV
        EXTERNAL		STR_ABBREV
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set default
      CONV = 1.0

*  Degrees
      IF ( STR_ABBREV( UNITS, 'DEG' ) ) THEN
        CONV = MATH__DTOR

*  Arcminutes
      ELSE IF ( STR_ABBREV( UNITS, 'ARCMIN' ) ) THEN
        CONV = MATH__DTOR / 60.0

*  Arcseconds
      ELSE IF ( STR_ABBREV( UNITS, 'ARCSEC' ) ) THEN
        CONV = MATH__DTOR / 3600.0

*  Radians
      ELSE IF ( STR_ABBREV( UNITS, 'RAD') ) THEN
        CONV = 1.0

*  Pixels!
      ELSE IF ( STR_ABBREV( UNITS, 'PIX') ) THEN
        CONV = 1.0

*  Otherwise duff
      ELSE
        CALL MSG_SETC( 'UN', UNITS )
        CALL ERR_REP( ' ', 'Unrecognised angular units ^UN', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'CONV_UNIT2R', STATUS )

      END
