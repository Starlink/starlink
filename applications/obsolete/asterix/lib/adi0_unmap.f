      SUBROUTINE ADI0_UNMAP( MODID, FILEID, PSID, STATUS )
*+
*  Name:
*     ADI0_UNMAP

*  Purpose:
*     Unmap an item given its private storage location

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI0_UNMAP( MODID, FILEID, PSID, STATUS )

*  Description:
*     Unmap an item given its private storage location. If the data is not
*     directly mapped to the file and the access mode is not read then an
*     optional WriteBack function is invoked.

*  Arguments:
*     MODID = INTEGER (given)
*        ADI identifier to top level model object
*     FILEID = INTEGER (given)
*        The ADI identifier of the file object
*     PSID = INTEGER (given)
*        ADI identifier to private storage
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER                   MODID,FILEID,PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*6		MODE, TYPE		! Mapping mode

      INTEGER			WB_CB			! WriteBack function

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get mode
      CALL ADI_CGET0C( PSID, 'Mode', MODE, STATUS )

*  Has a WriteBack function been provided?
      CALL ADI_THERE( PSID, 'WriteBack', THERE, STATUS )
      IF ( THERE ) THEN

*    Get writeback address
        CALL ADI_CGET0I( PSID, 'WriteBack', WB_CB, STATUS )

*    Invoke the write back
        CALL ADI0_DOWB( %VAL(WB_CB), MODID, FILEID, PSID, STATUS )

*  Issue warning because data mapped for update or write access is not
*  being handled cleanly
      ELSE IF ( MODE .NE. 'READ' ) THEN
        CALL MSG_PRNT( 'WARNING : Altered data cannot be written '/
     :                 /'back to file' )

      END IF

*  Force map count to zero
      CALL ADI_CPUT0I( PSID, 'MapCount', 0, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI0_UNMAP', STATUS )
      END IF

      END
