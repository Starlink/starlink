      SUBROUTINE DYN0_UNMAP( SLOT, STATUS )
*+
*  Name:
*     DYN0_UNMAP

*  Purpose:
*     Unmap an area of memory and free associated resources

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN0_UNMAP( SLOT, STATUS )

*  Description:
*     Frees memory associated with a particular DYN slot. If the memory
*     is mapped from a file then the file is deleted, otherwise the
*     memory is simply returned to the heap.

*  Arguments:
*     SLOT = INTEGER (given)
*        Internal slot number
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
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Mar 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'

*  Global Variables:
      INCLUDE 'DYN_CMN'                                 ! DYN common block
*       DYS_PTR[] = INTEGER (given and returned)
*         Number of items in a memory area
*       DYS_NITEM[] = INTEGER (given and returned)
*         Number of items in a memory area
*       DYS_FID[] = INTEGER (given and returned)
*         Number of items in a memory area

*  Arguments Given:
      INTEGER			SLOT			! Internal slot number

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! HDS file handle
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  File based memory?
      IF ( DYS_FID(SLOT) .NE. ADI__NULLID ) THEN

*    Extract locator
        CALL ADI1_GETLOC( DYS_FID(SLOT), LOC, STATUS )

*    Unmap the memory
        CALL DYN_UNMAP( LOC, STATUS )

*    Delete the file
        CALL HDS_ERASE( LOC, STATUS )

*    Free the ADI object
        CALL ADI_ERASE( DYS_FID(SLOT), STATUS )

*  Otherwise heap
      ELSE

        CALL PSX_FREE( DYS_PTR(SLOT), STATUS )

      END IF

*  Zero the slot regardless of status
      DYS_PTR(SLOT) = 0
      DYS_FID(SLOT) = ADI__NULLID

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DYN0_UNMAP', STATUS )

      END
