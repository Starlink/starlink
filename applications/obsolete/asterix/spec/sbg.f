      SUBROUTINE SBG( STATUS )
*+
*  Name:
*     SBG

*  Purpose:
*     Writes reference to background dataset into source dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL SBG( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Writes the logical BGND file link to the specified source dataset.
*     This link can be used subsequently by ASTERIX to load the background
*     dataset automatically. Supplying a null value for the background
*     dataset results in the file link being deleted.

*  Usage:
*     sbg {parameter_usage}

*  Environment Parameters:
*     INP = UNIV (update)
*        Source dataset
*     BGFILE = UNIV (read)
*        Background dataset

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

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     sbg, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 Jul 1992 V1.6-1 (TJP):
*        Original version.
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     28 Jul 1995 V2.0-0 (DJA):
*        Use new FRI routine
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'SBG Version 2.2-0' )

*  Local Variables:
      INTEGER			IFID			! Source dataset
      INTEGER			BFID			! Background dataset
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get names of source and background datasets
      CALL USI_ASSOC( 'INP', 'BinDS', 'UPDATE', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL USI_ASSOC( 'BGFILE', 'BinDS', 'READ', BFID, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
        BFID = ADI__NULLID

*  Abort on other bad status values
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99

      END IF

*  Make the link
      CALL FRI_PUTI( IFID, 'BGND', BFID, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
