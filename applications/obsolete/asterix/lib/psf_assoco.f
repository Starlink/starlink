      SUBROUTINE PSF_ASSOCO( FID, PSID, STATUS )
*+
*  Name:
*     PSF_ASSOCO

*  Purpose:
*     Associate a dataset and create psf data structure

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF_ASSOCO( FID, PSID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier to file to be associated
*     PSID = INTEGER (returned)
*        ADI identifier to psf storage area
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
*      5 Feb 1990 (DJA):
*        Original version
*     29 Jan 1994 (DJA):
*        Initialisation via block data
*      8 May 1996 (DJA):
*        Use PSID rather than slot number
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
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( PSF__PKG ) ) CALL PSF_INIT( STATUS )

*  Grab slot
      CALL PSF_GETSLOT( FID, PSID, STATUS )

*  Get library and routine name from user
      CALL PSF_PROMPT( .FALSE., ' ', PSID, STATUS )

*  Initialise the PSF routine
      CALL PSF_SLOTINIT( PSID, STATUS )

*  Try to write model to file
      CALL PSF_PUT_MODEL( PSID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PSF_ASSOCO', STATUS )

      END
