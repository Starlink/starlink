      REAL FUNCTION PSF0_GETAXDR( PSID, AX, STATUS )
*+
*  Name:
*     PSF0_GETAXDR

*  Purpose:
*     Get axis natural width from psf storage

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = PSF0_GETAXDR( PSID, AX, STATUS )

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
*     PSF0_GETAXDR = REAL
*        The axis basic width in radians

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
      REAL			DR			! Axis datum

      INTEGER			AXID			! Axis identifier
*.

*  Locate the axis
      CALL ADI_CCELL( PSID, 'Axes', 1, AX, AXID, STATUS )

*  Extract the datum
      CALL ADI_CGET0R( AXID, 'Dr', DR, STATUS )

*  Release the axis
      CALL ADI_ERASE( AXID, STATUS )

*  Set return value
      PSF0_GETAXDR = SCALE

      END
