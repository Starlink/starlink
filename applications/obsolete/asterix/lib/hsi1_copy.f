      SUBROUTINE HSI1_COPY( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI1_COPY

*  Purpose:
*     Copy history from one HDS file to another

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI1_COPY( NARG, ARGS, OARG, STATUS )

*  Description:
*     Copies the entire HISTORY structure from one HDS file to another, if
*     it exists. It is not an error for the structure to be absent. Any
*     existing history in the output file is deleted.

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
*     Creates a PSF_SLOT property on the property list of the first
*     argument if one is not already present.

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     DAT:
*        DAT_ANNUL	- Release an HDS locator
*        DAT_ERASE	- Erase an an HDS component
*        DAT_FIND	- Find an HDS component
*        DAT_COPY	- Copy an HDS component
*        DAT_THERE	- Does an HDS component exist?

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
*     14 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants
      INCLUDE 'DAT_PAR'					! HDS constants

*  Arguments Given:
      INTEGER			NARG			! # arguments
      INTEGER			ARGS(*)			! Method arguments

*  Arguments Returned:
      INTEGER			OARG			! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	HLOC			! Input HISTORY object
      CHARACTER*(DAT__SZLOC)	ILOC			! Input object
      CHARACTER*(DAT__SZLOC)	OLOC			! Output object

      LOGICAL			THERE			! Exists in input?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No output from this method
      OARG = ADI__NULLID

*  Extract first locator
      CALL ADI2_GETLOC( ARGS(1), ILOC, STATUS )

*  History exists?
      CALL DAT_THERE( ILOC, 'HISTORY', THERE, STATUS )
      IF ( THERE ) THEN

*    Extract output locator
        CALL ADI2_GETLOC( ARGS(2), OLOC, STATUS )

*    Delete existing history in output
        CALL DAT_THERE( OLOC, 'HISTORY', THERE, STATUS )
        IF ( THERE ) THEN
          CALL DAT_ERASE( OLOC, 'HISTORY', STATUS )
        END IF

*    Copy that component
        CALL DAT_FIND( ILOC, 'HISTORY', HLOC, STATUS )
        CALL DAT_COPY( HLOC, OLOC, 'HISTORY', STATUS )
        CALL DAT_ANNUL( HLOC, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI1_COPY', STATUS )
      END IF

      END
