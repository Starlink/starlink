      SUBROUTINE EDI_MAP<T>( ID, LISTS, MODE, LINDEX, HINDEX,
     :                       PTRS, STATUS )
*+
*  Name:
*     EDI_MAP<T>

*  Purpose:
*     Map the named lists as type <COMM> and mode

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_MAP<T>( ID, LISTS, MODE, LINDEX, HINDEX, PTRS, STATUS )

*  Description:
*     Maps the lists specified by the LISTS string with a type and mode
*     specified by TYPE and MODE. The pointers to the resulting areas
*     of memory are returned in PTRS.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     LISTS = CHARACTER*(*) (given)
*        List of lists to be mapped
*     MODE = CHARACTER*(*) (given)
*        The access mode for the lists' data
*     LINDEX = INTEGER (given)
*        The first list element to access
*     HINDEX = INTEGER (given)
*        The last list element to access
*     PTRS[] = INTEGER (returned)
*        The pointers to the mapped lists
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

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

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			ID, LINDEX, HINDEX
      CHARACTER*(*)		LISTS, MODE

*  Arguments Returned:
      INTEGER			PTRS(*)

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke master mapper
      CALL EDI_MAP( ID, LISTS, '<HTYPE>', MODE, LINDEX, HINDEX,
     :              PTRS, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_MAP<T>', STATUS )

      END
