      SUBROUTINE HSI2_GETVRB( HID, IVERB, STATUS )
*+
*  Name:
*     HSI2_GETVRB

*  Purpose:
*     Get history verbosity for this FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI2_GETVRB( HID, IVERB, STATUS )

*  Description:
*     Gets history verbosity for a history extension.

*  Arguments:
*     HID = INTEGER (given)
*        ADF locator to HISTORY extension
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
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private, history, verbosity

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Mar 1995 (DJA):
*        Original version.
*     20 Nov 1997 (RB):
*        Converted to FITS.
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
      INTEGER			HID			! HISTORY extension identifier

*  Arguments Returned:
      INTEGER			IVERB			! Verbosity

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*80		VERB			! Character verbosity
      CHARACTER*80		CMNT			! Comment field
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Does UPDATE_MODE exist?
      CALL ADI2_HGKYC( HID, 'UPDATE', VERB, CMNT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
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
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI2_GETVRB', STATUS )

      END
