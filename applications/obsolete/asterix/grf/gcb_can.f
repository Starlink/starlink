      SUBROUTINE GCB_CAN( NAMES, STATUS )
*+
*  Name:
*     GCB_CAN

*  Purpose:
*     Cancel multiple scalar attribute(s) of any type

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB_CAN( NAMES, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NAMES = CHARACTER*(*) (given)
*        Comma separated list of GCB item names
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
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     30 Nov 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GCB_PAR'

*  Global Variables:
      INCLUDE 'GCB_CMN'                                 ! GCB common block
*       G_MEMPTR = INTEGER (given)
*         Current GCB address

*  Arguments Given:
      CHARACTER*(*)		NAMES

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*16 		FMT,TYPE

      INTEGER			C1, C2			! Current name limits
      INTEGER 			DISP,SIZ
      INTEGER			INAME			! Current name number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create iterator over names
      CALL UDI0_CREITI( NAMES, C1, C2, INAME, STATUS )

*  Loop over items while more of them and status is ok
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Locate the named scalar
        CALL GCB_LOCSCAL( NAMES(C1:C2), DISP, SIZ, FMT, TYPE, STATUS )

*    Cancel it
        CALL GCB_CAN_SUB( %VAL(G_MEMPTR), DISP, SIZ, STATUS )

*    Advance iterator to next item
        CALL UDI0_ADVITI( NAMES, C1, C2, INAME, STATUS )

*  Next name
      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB_CAN', STATUS )

      END
