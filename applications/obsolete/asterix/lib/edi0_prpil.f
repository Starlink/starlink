      CHARACTER*8 FUNCTION EDI0_PRPIL( NUMBER )
*+
*  Name:
*     EDI0_PRPIL

*  Purpose:
*     Return a property name given a list number

*  Language:
*     Starlink Fortran

*  Invocation:
*     CHARACTER*8 STRING = EDI0_SETLNK( NUMBER )

*  Description:
*     Return a property name given a list number

*  Arguments:
*     NUMBER = INTEGER (given)
*        The list number

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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER                   NUMBER

*  Local Variables:
      CHARACTER*8		PNAME			! Property name
*.

*  Write a property encoding the list number in its name
      WRITE( PNAME, '(A,I2.2)' ) '.LIST_', NUMBER

*  Set return value
      EDI0_PRPIL = PNAME

      END
