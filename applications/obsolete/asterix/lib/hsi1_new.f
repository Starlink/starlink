      SUBROUTINE HSI1_NEW( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI1_NEW

*  Purpose:
*     Create new history in an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI1_NEW( NARG, ARGS, OARG, STATUS )

*  Description:
*     Creates the HISTORY structure in an HDS file to another. Existing
*     structure is deleted if present.

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
*        CMP_PUT0x	- Write value of HDS component
*        DAT_ANNUL	- Release an HDS locator
*        DAT_ERASE	- Erase an an HDS component
*        DAT_FIND	- Find an HDS component
*        DAT_NEW[0x]    - Create new HDS component
*        DAT_THERE	- Does an HDS component exist?

*  Implementation Deficiencies:
*     The constants in this routine should be loadable resources. This
*     would enable the user to turn off all history creation (somehow).

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

*  Local Constants:
      CHARACTER*8		UMODE
        PARAMETER		( UMODE = 'NORMAL' )
      INTEGER			ESIZE
        PARAMETER		( ESIZE = 10 )

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	HLOC			! Input HISTORY object
      CHARACTER*(DAT__SZLOC)	LOC			! Output HDS object

      CHARACTER*18		TSTR			! Time string

      LOGICAL			THERE			! Exists already?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No output from this method
      OARG = ADI__NULLID

*  Extract locator
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )

*  History exists?
      CALL DAT_THERE( LOC, 'HISTORY', THERE, STATUS )
      IF ( THERE ) THEN

*    Delete existing history in output
        CALL DAT_ERASE( LOC, 'HISTORY', STATUS )
        CALL MSG_PRNT( 'Erasing existing history structure...' )

      END IF

*  Create and locate new structure
      CALL DAT_NEW( LOC, 'HISTORY', 'HISTORY', 0, 0, STATUS )
      CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )

*  Create sub-components
      CALL DAT_NEW0C( HLOC, 'CREATED', 18, STATUS )
      CALL DAT_NEW0C( HLOC, 'UPDATE_MODE', 10, STATUS )
      CALL DAT_NEW0I( HLOC, 'EXTEND_SIZE', STATUS )
      CALL DAT_NEW0I( HLOC, 'CURRENT_RECORD', STATUS )
      CALL DAT_NEW( HLOC, 'RECORDS', 'HIST_REC', 1, ESIZE, STATUS )

*  Get time string
      CALL HSI0_TIME( TSTR, STATUS )

*  Fill values of these objects
      CALL CMP_PUT0C( HLOC, 'CREATED', TSTR, STATUS )
      CALL CMP_PUT0C( HLOC, 'UPDATE_MODE', UMODE, STATUS )
      CALL CMP_PUT0I( HLOC, 'EXTEND_SIZE', ESIZE, STATUS )
      CALL CMP_PUT0I( HLOC, 'CURRENT_RECORD', 0, STATUS )

*  Free top-level structure
      CALL DAT_ANNUL( HLOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI1_NEW', STATUS )
      END IF

      END
