      SUBROUTINE HSI1_GETVRB( HLOC, IVERB, STATUS )
*+
*  Name:
*     HSI1_GETVRB

*  Purpose:
*     Get history verbosity for this file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI1_GETVRB( HLOC, IVERB, STATUS )

*  Description:
*     Gets history verbosity for a history structure.

*  Arguments:
*     HLOC = CHARACTER*(DAT__SZLOC) (given)
*        Locator to a HISTORY structure
*     IVERB = INTEGER (retirned)
*        Verbosity level
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
*     package:hsi, usage:private, history, verbosity

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Mar 1995 (DJA):
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
      INCLUDE 'HSI_PAR'

*  Arguments Given:
      CHARACTER*(DAT__SZLOC)	HLOC			! History structure

*  Arguments Returned:
      INTEGER			IVERB			! Verbosity

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*10		VERB			! Character verbosity

      LOGICAL			THERE			! Component exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Does UPDATE_MODE exist?
      CALL DAT_THERE( HLOC, 'UPDATE_MODE', THERE, STATUS )
      IF ( THERE ) THEN
        CALL CMP_GET0C( HLOC, 'UPDATE_MODE', VERB, STATUS )
        IF ( VERB .EQ. 'DISABLED' ) THEN
          IVERB = HSI__DISABLED
        ELSE IF ( VERB .EQ. 'QUIET' ) THEN
          IVERB = HSI__QUIET
        ELSE IF ( VERB .EQ. 'NORMAL' ) THEN
          IVERB = HSI__NORMAL
        ELSE IF ( VERB .EQ. 'VERBOSE' ) THEN
          IVERB = HSI__VERBOSE
        ELSE
          CALL MSG_SETC( 'VERB', VERB )
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Unrecognised history verbosity, '/
     :                  /'NORMAL assumed', STATUS )
          CALL ERR_FLUSH( STATUS )
        END IF

      ELSE
        IVERB = HSI__NORMAL

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI1_GETVRB', STATUS )

      END
