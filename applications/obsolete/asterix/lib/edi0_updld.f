      SUBROUTINE EDI0_UPDLD( EID, LID, STATUS )
*+
*  Name:
*     EDI0_UPDLD

*  Purpose:
*     Update and EventDS structure with a new EventList

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI0_UPDLD( EID, LID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     EID = INTEGER (given)
*        ADI identifier of EventDS (or derived) object
*     LID = INTEGER (given)
*        ADI identifier of EventList (or derived) object
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
*     18 Aug 1995 (DJA):
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
      INTEGER			EID, LID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			EDI0_PRPIL
        CHARACTER*8		EDI0_PRPIL

*  Local Variables:
      CHARACTER*20		NAME			! List name
      CHARACTER*8		PNAME			! Property name

      INTEGER			L			! List name length
      INTEGER			LSID			! List container
      INTEGER			NLIST			! # lists

      LOGICAL			THERE			! List already exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get list name of new list
      CALL ADI_CGET0C( LID, 'Name', NAME, STATUS )
      L = CHR_LEN(NAME)

*  Locate list container
      CALL ADI_FIND( EID, 'Lists', LSID, STATUS )

*  Does it already exist in EventDS?
      CALL ADI_THERE( LSID, NAME(:L), THERE, STATUS )
      IF ( THERE ) THEN

*    Extract number property
        CALL ADI_CGET0I( LID, '.Number', NLIST, STATUS )

*    Erase existing list structure
        CALL ADI_CERASE( LSID, NAME(:L), STATUS )

*    Write number property to list description
        CALL ADI_CPUT0I( LID, '.Number', NLIST, STATUS )

*    Write new list structure
        CALL ADI_CPUTID( LSID, NAME(:L), LID, STATUS )

*  Define new list
      ELSE

*    Get number of lists
        CALL ADI_CGET0I( EID, 'NLIST', NLIST, STATUS )

*    Increment it
        NLIST = NLIST + 1

*    Find property name
        PNAME = EDI0_PRPIL( NLIST )

*    Write number property to list description
        CALL ADI_CPUT0I( LID, '.Number', NLIST, STATUS )

*    Write new list structure
        CALL ADI_CPUTID( LSID, NAME(:L), LID, STATUS )

*    Write back the new number of lists
        CALL ADI_CPUT0I( EID, 'NLIST', NLIST, STATUS )

*    Write the property with the name of the list as value
        CALL ADI_CPUT0C( LSID, PNAME, NAME(:L), STATUS )

*  End of list exists test
      END IF

*  Free the list container
      CALL ADI_ERASE( LSID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI0_UPDLD', STATUS )

      END
