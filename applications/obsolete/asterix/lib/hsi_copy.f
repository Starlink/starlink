      SUBROUTINE HSI_COPY( IFID, OFID, STATUS )
*+
*  Name:
*     HSI_COPY

*  Purpose:
*     Copy history from first dataset to the second

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI_COPY( IFID, OFID, STATUS )

*  Description:
*     Copy all history records from the first dataset to the second.

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier to input dataset
*     OFID = INTEGER (given)
*        ADI identifier to output dataset
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
*     package:hsi, usage:public, history, copying

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
*        Original version.
*     14 Mar 1995 (DJA):
*        Now works using ADI method.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  Global Variables:
      INCLUDE 'HSI_CMN'                 		! HSI common block
*       HSI_INIT = LOGICAL (given)
*         HSI class definitions loaded?

*  Arguments Given:
      INTEGER			IFID			! Input dataset
      INTEGER			OFID			! Output dataset

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL                  HSI0_BLK                ! Ensures inclusion

*  Local Variables:
      INTEGER			IARG(2)			! Method inputs
      INTEGER			OARG			! Output from method
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. HSI_INIT ) CALL HSI0_INIT( STATUS )

*  Invoke the CopyHistory method
      IARG(1) = IFID
      IARG(2) = OFID
      CALL ADI_EXEC( 'CopyHistory', 2, IARG, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI_COPY', STATUS )

      END
