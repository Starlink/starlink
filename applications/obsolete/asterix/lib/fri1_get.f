      SUBROUTINE FRI1_GET( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     FRI1_GET

*  Purpose:
*     Extract value of file reference

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI1_GET( NARG, ARGS, OARG, STATUS )

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
*     FRI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fri.html

*  Keywords:
*     package:fri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     1 Aug 1995 (DJA):
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
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)    ALOC                    ! ASTERIX locator
      CHARACTER*(DAT__SZLOC)    ARLOC                   ! AST_REF object
      CHARACTER*(DAT__SZNAM)    ROBJ                    ! Reference obj name

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate ASTERIX box of file
      CALL ADI1_LOCAST( ARGS(1), .FALSE., ALOC, STATUS )

*  Translate the logical name into the HDS will use
      CALL FRI1_TRNSNM( ARGS(2), ROBJ, STATUS )

*  Create output structure
      CALL ADI_NEW0( 'STRUC', OARG, STATUS )

*  Reference exists?
      CALL DAT_THERE( ALOC, ROBJ, THERE, STATUS )
      IF ( THERE ) THEN

*    Locate the reference
        CALL DAT_FIND( ALOC, ROBJ, ARLOC, STATUS )

*    Copy components
        CALL ADI_CCH2AC( ARLOC, 'FILE', OARG, 'File', STATUS )
        CALL ADI_CCH2AC( ARLOC, 'PATH', OARG, 'Path', STATUS )

*    Release reference
        CALL DAT_ANNUL( ARLOC, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI1_GET', STATUS )

      END
