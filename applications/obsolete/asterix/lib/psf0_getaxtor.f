      REAL FUNCTION PSF0_GETAXTOR( PSID, AX, STATUS )
*+
*  Name:
*     PSF0_GETAXTOR

*  Purpose:
*     Get axis radian conversion factor from psf storage

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = PSF0_GETAXTOR( PSID, AX, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier of psf storage
*     AX = INTEGER (given)
*        The axis number of interest
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Returned Value:
*     PSF0_GETAXTOR = REAL
*        The factor which converts axis units to radians

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
*     {facility_or_package}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:private

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      8 May 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER			PSID, AX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL			TOR			! Axis datum
      INTEGER			AXID			! Axis identifier
*.

*  Locate the axis
      CALL ADI_CCEL( PSID, 'Axes', 1, AX, AXID, STATUS )

*  Extract the datum
      CALL ADI_CGET0R( AXID, 'Tor', TOR, STATUS )

*  Release the axis
      CALL ADI_ERASE( AXID, STATUS )

*  Set return value
      PSF0_GETAXTOR = TOR

      END
