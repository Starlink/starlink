      SUBROUTINE BDI_MAP<T>( ID, ITEMS, MODE, PTRS, STATUS )
*+
*  Name:
*     BDI_MAP<T>

*  Purpose:
*     Map the named items as <COMM> with the specified mode

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_MAP<T>( ID, ITEMS, MODE, PTRS, STATUS )

*  Description:
*     Maps the items specified by the ITEMS string with a type of
*     <COMM> and mode specified by MODE. The pointers to the resulting
*     areas of memory are returned in PTRS.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be mapped
*     MODE = CHARACTER*(*) (given)
*        The access mode for the items
*     PTRS[] = INTEGER (returned)
*        The pointers to the mapped items
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
*     package:bdi, usage:public

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
      INTEGER                   ID
      CHARACTER*(*)             ITEMS, MODE

*  Arguments Returned:
      INTEGER                   PTRS(*)

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke
      CALL BDI_MAP( ID, ITEMS, '<HTYPE>', MODE, PTRS, STATUS )

      END
