      SUBROUTINE PSF0_GETID0<T>( PSID, NAME, VALUE, STATUS )
*+
*  Name:
*     PSF0_GETID0<T>

*  Purpose:
*     Get value of the <COMM> psf instance data member called NAME

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF0_GETID0<T>( PSID, NAME, VALUE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier of PsfDescription object
*     NAME = CHARACTER*(*) (given)
*        Name of InstanceData member
*     VALUE = <TYPE> (returned)
*        Value of InstanceData member
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
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Mar 1996 (DJA):
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
      INTEGER			PSID
      CHARACTER*(*)		NAME

*  Arguments Returned:
      <TYPE>			VALUE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			IID			! InstanceData struct
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the instance data structure
      CALL ADI_FIND( PSID, 'InstanceData', IID, STATUS )

*  Write the data
      CALL ADI_CGET0<T>( IID, NAME, VALUE, STATUS )

*  Release the instance data
      CALL ADI_ERASE( IID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF0_GETID0<T>', STATUS )
      END IF

      END
