      SUBROUTINE BDI_GET1<T>( ID, ITEMS, DIMX, DATA, DIM, STATUS )
*+
*  Name:
*     BDI_GET1<T>

*  Purpose:
*     Get the named items as <COMM> with specified dimensions

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_GET1<T>( ID, ITEMS, DIMX, DATA, DIM, STATUS )

*  Description:
*     Retrieves the items specified by the ITEMS string as <COMM>
*     values.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be read
*     DIMX = INTEGER (given)
*        The dimension of the users buffer, per item
*     DATA[] = <TYPE> (returned)
*        The returned data
*     DIM = INTEGER (returned)
*        The actual sizes of the data written the users buffer
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
      INTEGER			ID, DIMX
      CHARACTER*(*)		ITEMS

*  Arguments Returned:
      <TYPE>			DATA(*)
      INTEGER			DIM

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Simply invoke generic routine
      CALL BDI_GET<T>( ID, ITEMS, 1, DIMX, DATA, DIM, STATUS )

      END
