      SUBROUTINE DYN_SIZE( PTR, SIZE, STATUS )
*+
*  Name:
*     DYN_SIZE

*  Purpose:
*     Return number of data elements in mapped memory

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN_SIZE( PTR, SIZE, STATUS )

*  Description:
*     Return number of data elements in an area mapped memory which has
*     been mapped by DYN. It is an error to supply this routine with an
*     address not allocated by DYN.

*  Arguments:
*     PTR = INTEGER (given)
*        Address of a memory section allocated by DYN
*     SIZE = INTEGER (returned)
*        Number of data elements in mapped memory
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     A rather unsavoury routine.

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
*     package:dyn, usage:public

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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'DYN_CMN'                                 ! DYN common block
*       DYN_ISINIT = LOGICAL (given)
*         DYN class definitions loaded?
*       DYS_NITEM[] = INTEGER (given and returned)
*         Number of items in a memory area

*  Arguments Given:
      INTEGER			PTR			! Memory address

*  Arguments Returned:
      INTEGER			SIZE			! Number of mapped items

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL                  DYN0_BLK                ! Ensures inclusion

*  Local Variables:
      INTEGER			SLOT			! Internal slot number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. DYN_ISINIT ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'DYN system has not been initialised', STATUS )
      END IF

*  Locate the memory
      CALL DYN0_FIND( PTR, SLOT, STATUS )

*  Return the number of items
      IF ( STATUS .EQ. SAI__OK ) THEN
        SIZE = DYS_NITEM(SLOT)
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DYN_SIZE', STATUS )

      END
