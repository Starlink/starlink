      SUBROUTINE PSF_GETSLOT( FID, PSID, STATUS )
*+
*  Name:
*     PSF_GETSLOT

*  Purpose:
*     Return psf storage identifier given file id

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF_GETSLOT( FID, PSID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        File identifier
*     PSID = INTEGER (returned)
*        Psf identifier
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
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 Nov 1989 (DJA):
*        Original version.
*      7 Dec 1992 (DJA):
*        Removed SIZEOF function for UNIX port
*      8 May 1996 (DJA):
*        Full ADI version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			FID

*  Arguments Returned:
      INTEGER			PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			AST_QPKGI
        LOGICAL			AST_QPKGI
      EXTERNAL			PSF_BLK

*  Local Constants:
      CHARACTER*4		PSF_PROP
        PARAMETER		( PROP = '.PSFID' )

*  Local Variables:
      LOGICAL			THERE			! Already exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( PSF__PKG ) ) CALL PSF_INIT( STATUS )

*  Property already exists?
      CALL ADI_THERE( FID, PROP, THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET0I( FID, PROP, PSID, STATUS )

*  New psf
      ELSE

*    Create psf object
        CALL ADI_NEW0( 'PsfDescription', PSID, STATUS )

*    Store file id
        CALL ADI_CPUT0I( PSID, 'FileID', FID, STATUS )

*    Store psf identifier
        CALL ADI_CPUT0I( FID, PROP, PSID, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PSF_GETSLOT', STATUS )

      END
