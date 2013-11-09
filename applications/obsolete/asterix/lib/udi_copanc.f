      SUBROUTINE UDI_COPANC( IFID, OMIT, OFID, STATUS )
*+
*  Name:
*     UDI_COPANC

*  Purpose:
*     Copy ancillary information from one dataset to another

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UDI_COPANC( IFID, OMIT, OFID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier of dataset to copy from
*     OMIT = CHARACTER*(*) (given)
*        List of things not to copy
*     OFID = INTEGER (given)
*        ADI identifier of dataset to copy to
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
*     UDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/udi.html

*  Keywords:
*     package:udi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Sep 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_ERR'

*  Arguments Given:
      INTEGER			IFID, OFID
      CHARACTER*(*)		OMIT

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			UDI0_COPANC

*  Local Variables:
      INTEGER			ARGS(5)			! Method arguments
      INTEGER			DID			! Dummy return value
      INTEGER			OARG			! Ignored method result

      LOGICAL			FIRST			! First time through?
        SAVE			FIRST

*  Local Data:
      DATA			FIRST/.TRUE./
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First item through?
      IF ( FIRST ) THEN
        CALL ADI_DEFMTH( 'CopyAncillary(_,_FITSfile,_,_HDSfile,_CHAR)',
     :                   UDI0_COPANC, DID, STATUS )
        CALL ADI_DEFMTH( 'CopyAncillary(_,_HDSfile,_,_FITSfile,_CHAR)',
     :                   UDI0_COPANC, DID, STATUS )
        FIRST = .FALSE.
      END IF

*  Get file identifiers for each input
      ARGS(1) = IFID
      CALL ADI_GETFILE( IFID, ARGS(2), STATUS )
      ARGS(3) = OFID
      CALL ADI_GETFILE( OFID, ARGS(4), STATUS )
      CALL ADI_NEWV0C( OMIT, ARGS(5), STATUS )

*  Invoke method
      CALL ADI_EXEC( 'CopyAncillary', 5, ARGS, OARG, STATUS )

*  Trap errors
      IF ( STATUS .EQ. ADI__NOMTH ) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_FLUSH( STATUS )
      END IF

*  Release omit string
      CALL ADI_ERASE( ARGS(5), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'UDI_COPANC', STATUS )

      END
