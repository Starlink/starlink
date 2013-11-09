      SUBROUTINE PSF0_GETAXTXT( PSID, AX, LABEL, UNITS, STATUS )
*+
*  Name:
*     PSF0_GETAXTXT

*  Purpose:
*     Get axis data from psf storage

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF0_GETAXTXT( PSID, AX, LABEL, UNITS, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier of psf storage
*     AX = INTEGER (given)
*        The axis number of interest
*     LABEL = CHARACTER*(*) (returned)
*        Axis data label
*     UNITS = CHARACTER*(*) (returned)
*        Axis data units
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
*     package:psf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     8 May 1996 (DJA):
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
      INTEGER			PSID, AX

*  Arguments Returned:
      CHARACTER*(*)		LABEL, UNITS

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			AXID			! Axis identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the axis
      CALL ADI_CCELL( PSID, 'Axes', 1, AX, AXID, STATUS )

*  Extract the data
      CALL ADI_CGET0C( AXID, 'Label', LABEL, STATUS )
      CALL ADI_CGET0C( AXID, 'Units', UNITS, STATUS )

*  Release the axis
      CALL ADI_ERASE( AXID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF0_GETAXTXT', STATUS )
      END IF

      END
