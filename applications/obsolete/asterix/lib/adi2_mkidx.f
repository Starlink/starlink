      CHARACTER*8 FUNCTION ADI2_MKIDX( ROOT, INDEX )
*+
*  Name:
*     ADI2_MKIDX

*  Purpose:
*     Make an index string given an index number

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = ADI2_MKIDX( ROOT, INDEX )

*  Description:
*     {routine_description}

*  Arguments:
*     ROOT = CHARACTER*(*)
*        The root of the name
*     INDEX = INTEGER (given)
*        The index to encode

*  Returned Value:
*     ADI2_MKIDX = CHARACTER*8
*        String containing indexable object name

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
*     {facility_or_package}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      4 Jun 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER*(*)		ROOT
      INTEGER			INDEX

*  Local Variables:
      CHARACTER*8		IDXSTR			! Index string
      CHARACTER*4		LROOT			! Local root copy
*.

*  Local copy
      LROOT = ROOT//'___'

*  Make the string
      WRITE( IDXSTR, '(A,I4.4)' ) LROOT, INDEX

*  Set return value
      ADI2_MKIDX = IDXSTR

      END
