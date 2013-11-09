      SUBROUTINE EDI1_QUNMAP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     EDI1_QUNMAP

*  Purpose:
*     Service ListUnmapQuantum requests from the EDI system for HDS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI1_QUNMAP( NARG, ARGS, OARG, STATUS )

*  Description:
*     Services EDI unmap quantum requests for HDS files.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER                   NARG
      INTEGER                   ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		LIST

      INTEGER			LID			! List structure
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), LIST, STATUS )

*  Locate the list structure
      CALL EDI_IDXNAM( ARGS(1), LIST, LID, STATUS )

*  Unmap the list quantum
      CALL EDI1_QUNMAP_INT( LID, STATUS )

*  And the list structure
      CALL ADI_ERASE( LID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI1_QUNMAP', STATUS )

      END



      SUBROUTINE EDI1_QUNMAP_INT( LID, STATUS )
*+
*  Name:
*     EDI1_QUNMAP_INT

*  Purpose:
*     Unmap an HDS list quantum given its ADI description

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI1_QUNMAP_INT( LID, STATUS )

*  Description:
*     Unmaps the HDS list specified by the input argument

*  Arguments:
*     LID = INTEGER (given)
*        ADI identifier of list description
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

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   LID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	SLOC			! Mapped object

      CHARACTER*20		LIST
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get value of the component holding the mapped locator
      CALL ADI_CGET0C( LID, '.MappedComponentQ', SLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ADI_NAME( LID, LIST, STATUS )
        CALL MSG_SETC( 'L', LIST )
        STATUS = SAI__ERROR
        CALL ERR_REP( 'EDI1_QUNMAP_1', 'List ^L quantum has not been '/
     :                /'mapped!', STATUS )
      END IF

*  Unmap the object
      CALL DAT_UNMAP( SLOC, STATUS )

*  Release the list mapped quantum locator
      CALL DAT_ANNUL( SLOC, STATUS )

*  Write property containing locator used for mapping. This is need to
*  unmap cleanly
      CALL ADI_CERASE( LID, '.MappedComponentQ', STATUS )

      END
