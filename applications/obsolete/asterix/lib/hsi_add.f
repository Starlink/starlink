      SUBROUTINE HSI_ADD( IFID, NAME, STATUS )
*+
*  Name:
*     HSI_ADD

*  Purpose:
*     Create new history record

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI_ADD( IFID, NAME, STATUS )

*  Description:
*     Create a new history record. The program creating the history is
*     named in the second argument.

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier of the dataset
*     NAME = CHARACTER*(*) (given)
*        The name of the program writing the history
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
*     package:hsi, usage:public, history, creation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
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
      INTEGER			IFID			! Dataset identifier
      CHARACTER*(*)		NAME			! Program name

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL                  HSI0_BLK                ! Ensures inclusion

*  Local Variables:
      INTEGER			IARG(2)			! Method inputs
      INTEGER			OARG			! Method output

      LOGICAL			TEMPOK			! Temp string created?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. HSI_INIT ) CALL HSI0_INIT( STATUS )

*  Store first argument
      IARG(1) = IFID

*  Temporary string for command name
      CALL ADI_NEWV0C( NAME, IARG(2), STATUS )
      TEMPOK = (STATUS.EQ.SAI__OK)

*  Invoke the method
      CALL ADI_EXEC( 'AddHistory', 2, IARG, OARG, STATUS )

*  Scrub temporary string
      IF ( TEMPOK ) THEN

*    New error context in case addition of history failed
        CALL ERR_BEGIN( STATUS )

*    Scrub the string
        CALL ADI_ERASE( IARG(2), STATUS )

*    Restore context
        CALL ERR_END( STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI_ADD', STATUS )

      END
