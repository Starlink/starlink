      SUBROUTINE GCB1_CSAVE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     GCB1_CSAVE

*  Purpose:
*     Save a cached GCB

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB1_CSAVE( NARG, ARGS, OARG, STATUS )

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
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Oct 1995 (DJA):
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
      INCLUDE 'GCB_PAR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	GCBLOC			! Locator to GCB

      INTEGER			CACHE			! Cache address
      INTEGER			GCBPTR			! Mapped file GCB
      INTEGER			NBYTE			! Bytes to store GCB
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract cache address
      CALL ADI_GET0I( ARGS(1), CACHE, STATUS )

*  Find size required
      CALL GCB0_CASIZE( CACHE, NBYTE, STATUS )

*  Locate and map the file GCB
      CALL GCB1_SAVE_MAPGCB( ARGS(2), NBYTE, GCBLOC, GCBPTR, STATUS )

*  Copy cache to output
      CALL ARR_COP1B( NBYTE, %VAL(CACHE), %VAL(GCBPTR), STATUS )

*  Release output
      CALL DAT_UNMAP( GCBLOC, STATUS )
      CALL DAT_ANNUL( GCBLOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB1_CSAVE', STATUS )

      END
