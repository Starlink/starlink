      SUBROUTINE DYN_UNMAP( PTR, STATUS )
*+
*  Name:
*     DYN_UNMAP

*  Purpose:
*     Unmap memory allocated by DYN

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN_UNMAP( PTR, STATUS )

*  Description:
*     Unmap memory allocated by DYN. It is an error to supply this routine
*     with an address not allocated by DYN.

*  Arguments:
*     PTR = INTEGER (given)
*        Address of a memory section allocated by DYN
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

*  Arguments Given:
      INTEGER			PTR			! Memory address

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
        CALL ERR_REP( ' ', 'DYN system has not been initialised',
     :                STATUS )
      END IF

*  Locate the memory. Prevent deallocation of null pointer
      IF ( PTR .NE. 0 ) THEN
        CALL DYN0_FIND( PTR, SLOT, STATUS )

*    Unmap it
        CALL DYN0_UNMAP( SLOT, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DYN_UNMAP', STATUS )

      END
