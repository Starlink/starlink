      SUBROUTINE HSI_PTXTI( IFID, LINES, DEL, STATUS )
*+
*  Name:
*     HSI_PTXTI

*  Purpose:
*     Write ADI text array to current history record

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI_PTXTI( IFID, LINES, DEL, STATUS )

*  Description:
*     Write text lines to current history record

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier of the dataset
*     LINES = INTEGER (given)
*        ADI identifier of string array
*     DEL = LOGICAL (given)
*        Delete ADI array after writing?
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
*     package:hsi, usage:public, history, creation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      4 Jan 1996 (DJA):
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
      INCLUDE 'HSI_CMN'                                 ! HSI common block
*       HSI_INIT = LOGICAL (given)
*         HSI class definitions loaded?

*  Arguments Given:
      INTEGER			IFID, LINES
      LOGICAL			DEL

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL                  HSI0_BLK                ! Ensures inclusion

*  Local Variables:
      INTEGER			IARG(2)			! Method inputs
      INTEGER			OARG			! Method output
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. HSI_INIT ) CALL HSI0_INIT( STATUS )

*  Get base file object
      CALL ADI_GETFILE( IFID, IARG(1), STATUS )

*  Temporary string for command name
      IARG(2) = LINES

*  Invoke the method
      CALL ADI_EXEC( 'AddHistoryText', 2, IARG, OARG, STATUS )

*  Delete array?
      IF ( DEL ) THEN
        CALL ADI_ERASE( LINES, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI_PTXTI', STATUS )

      END
