      SUBROUTINE EDI1_UNLNK( LHS, RHS, STATUS )
*+
*  Name:
*     EDI1_UNLNK

*  Purpose:
*     Service UnLink method for EventDS to HDSfile links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI1_UNLNK( LHS, RHS, STATUS )

*  Description:
*     Performs tidying up prior to ADI file link between high level objects
*     EventDS and HDSfile being destroyed.

*  Arguments:
*     LHS = INTEGER (given)
*        ADI identifier of high level object
*     RHS = INTEGER (given)
*        ADI identifier of low level object
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
      INTEGER                   LHS, RHS

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			EDI0_PRPIL
        CHARACTER*8             EDI0_PRPIL

*  Local Variables:
      INTEGER			I			! Loop over lists
      INTEGER			LID			! Lists object id
      INTEGER			NEVENT			! Number of records
      INTEGER			NLIST			! Number of lists

      LOGICAL			THERE			! Object is mapped?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get number of lists
      CALL EDI_GETNS( LHS, NEVENT, NLIST, STATUS )

*  Loop over lists, unmapping those which are still mapped
      DO I = 1, NLIST

*    Locate the list
        CALL EDI_IDX( LHS, I, LID, STATUS )

*    Is it mapped?
        CALL ADI_THERE( LID, '.MappedComponent', THERE, STATUS )
        IF ( THERE ) THEN
          CALL EDI1_UNMAP_INT( LID, STATUS )
        END IF

*    Free the list
        CALL ADI_ERASE( LID, STATUS )

      END DO

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI1_UNLNK', STATUS )

*  Invoke base method to perform linkage
      CALL ADI_CALNXT( STATUS )

      END
