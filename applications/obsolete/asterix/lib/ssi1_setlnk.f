      SUBROUTINE SSI1_SETLNK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     SSI1_SETLNK

*  Purpose:
*     Link an SSDS to an existing HDSfile

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SSI1_SETLNK( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

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
*     SSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/ssi.html

*  Keywords:
*     package:ssi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Feb 1996 (DJA):
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
      INCLUDE 'ADI_PAR'
      INCLUDE 'ADI_ERR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Dataset locator
      CHARACTER*(DAT__SZLOC)	TYP			! Top-level type
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set return argument
      OARG = ADI__NULLID

*  Get locator
      CALL ADI1_GETLOC( ARGS(2), LOC, STATUS )

*  Get type
      CALL DAT_TYPE( LOC, TYP, STATUS )

*  Correct type?
      IF ( TYP .EQ. 'SSDS' ) THEN
        CALL ADI_SETLNK( ARGS(1), ARGS(2), STATUS )

      ELSE

*    Allow retry if not an SSDS
        STATUS = ADI__RETRY
        CALL ERR_REP( ' ', 'Input is not a source search results file',
     :                STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SSI1_SETLNK', STATUS )

      END



      SUBROUTINE SSI1_SSETLNK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     SSI1_SSETLNK

*  Purpose:
*     Link an SSDSset to an existing HDSfile

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SSI1_SETLNK( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

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
*     SSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/ssi.html

*  Keywords:
*     package:ssi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Feb 1996 (DJA):
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
      INCLUDE 'ADI_PAR'
      INCLUDE 'ADI_ERR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Dataset locator
      CHARACTER*(DAT__SZLOC)	TYP			! Top-level type

      INTEGER			NDIM			! Object dimensionality
      INTEGER			NFILE			! # book components

      LOGICAL			OK			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set return argument
      OARG = ADI__NULLID

*  Get locator
      CALL ADI1_GETLOC( ARGS(2), LOC, STATUS )

*  Get type
      CALL DAT_TYPE( LOC, TYP, STATUS )

*  Correct type?
      IF ( TYP .EQ. 'SSDS_SET' ) THEN
        CALL ADI_SETLNK( ARGS(1), ARGS(2), STATUS )

*    Determine how many files contributed to this set
*    BOOK exists?
        CALL DAT_THERE( LOC, 'BOOK', OK, STATUS )

*    Get size
        IF ( OK ) THEN
          CALL CMP_SHAPE( LOC, 'BOOK', 1, NFILE, NDIM, STATUS )
        ELSE
          NFILE = 0
        END IF
        CALL ADI_CPUT0I( ARGS(1), 'NFILE', NFILE, STATUS )

      ELSE

*    Allow retry if not an SSDS
        STATUS = ADI__RETRY
        CALL ERR_REP( ' ', 'Input is not a source search results file',
     :                STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SSI1_SSETLNK', STATUS )
      END IF

      END
