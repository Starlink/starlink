      SUBROUTINE ADI2_ARYWB( MODID, FILID, PSID, STATUS )
*+
*  Name:
*     ADI2_ARYWB

*  Purpose:
*     Write back an array to an FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ARYWB( MODID, FILID, PSID, STATUS )

*  Description:
*     This routine is called before the association between a bit of memory
*     and an FITS object is destroyed.

*  Arguments:
*     MODID = INTEGER (given)
*        ADI identifier to top level model object
*     FILID = INTEGER (given)
*        The ADI identifier of the FITS file object
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
*     This routine coerces to the simple array representations, but there
*     should be some mechanism for handling magic values and writing the
*     appropriate flags.

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
*     9 Aug 1995 (DJA):
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
      INTEGER                   MODID,FILID,PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			CACHEID			! Cache object
      INTEGER			PTR			! Item data address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract information required to free cache object
      CALL ADI_CGET0I( PSID, 'CacheID', CACHEID, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )

*  Unmap the data
      CALL ADI_CUNMAP( CACHEID, 'Value', PTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_ARYWB', STATUS )
      END IF

      END
