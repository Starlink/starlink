      SUBROUTINE BDI_PUT1<T>( ID, ITEMS, DIM, DATA, STATUS )
*+
*  Name:
*     BDI_PUT1<T>

*  Purpose:
*     Put the named vector items with the type <COMM>

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_PUT1<T>( ID, ITEMS, DIM, DATA, STATUS )

*  Description:
*     Writes the items specified by the ITEMS string with the type <COMM>

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be mapped
*     DIM = INTEGER (given)
*        The dimension of the users buffer
*     DATA[] = <TYPE> (given)
*        The returned data
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
      INTEGER			ID, DIM
      CHARACTER*(*)		ITEMS
      <TYPE>			DATA(*)

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke basic routine
      CALL BDI_PUT<T>( ID, ITEMS, 1, DIM, DATA, STATUS )

      END
