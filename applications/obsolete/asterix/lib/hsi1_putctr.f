      SUBROUTINE HSI1_PUTCTR( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI1_PUTCTR

*  Purpose:
*     Write history control data to a dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI1_PUTCTR( NARG, ARGS, OARG, STATUS )

*  Description:
*     Writes information contained in a HistoryControl object to the
*     specified dataset.

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
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     16 Mar 1995 (DJA):
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*10		CVERB			! Verbosity mode
      CHARACTER*(DAT__SZLOC)	HLOC			! HISTORY object
      CHARACTER*(DAT__SZLOC)	LOC			! Dataset locator

      LOGICAL			THERE			! Component exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Null output from this method
      OARG = ADI__NULLID

*  Extract locator of dataset
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )

*  Locate HISTORY structure?
      CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Write the verbosity
        CALL ADI_CGET0C( ARGS(2), 'Verbosity', CVERB, STATUS )
        CALL DAT_THERE( HLOC, 'UPDATE_MODE', THERE, STATUS )
        IF ( .NOT. THERE ) THEN
          CALL DAT_NEW0C( HLOC, 'UPDATE_MODE', 10, STATUS )
        END IF
        CALL CMP_GET0C( HLOC, 'UPDATE_MODE', CVERB, STATUS )

*    Release top-level object
        CALL DAT_ANNUL( HLOC, STATUS )

      ELSE
        CALL ERR_ANNUL( STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI1_PUTCTR', STATUS )

      END
