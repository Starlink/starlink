      SUBROUTINE PSF0_GETAXVAL( PSID, AX, DIM, REG, PTR, BASE, SCALE,
     :                          TOR, STATUS )
*+
*  Name:
*     PSF0_GETAXVAL

*  Purpose:
*     Get axis data from psf storage

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF0_GETAXVAL( PSID, AX, DIM, REG, PTR, BASE, SCALE, TOR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier of psf storage
*     AX = INTEGER (given)
*        The axis number of interest
*     DIM = INTEGER (returned)
*        Axis size
*     REG = LOGICAL (returned)
*        Axis values regularly spaced?
*     PTR = INTEGER (returned)
*        Axis data pointer if REG false
*     BASE = REAL (returned)
*        Axis data base
*     SCALE = REAL (returned)
*        Axis data scale
*     TOR = REAL (returned)
*        Conversion factor to radians
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
      INTEGER			DIM, PTR
      LOGICAL			REG
      REAL			BASE, SCALE, TOR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			AXID			! Axis identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the axis
      CALL ADI_CCEL( PSID, 'Axes', 1, AX, AXID, STATUS )

*  Extract the data
      CALL ADI_CGET0I( AXID, 'Dim', DIM, STATUS )
      CALL ADI_CGET0L( AXID, 'Reg', REG, STATUS )
      CALL ADI_CGET0I( AXID, 'Ptr', PTR, STATUS )
      CALL ADI_CGET0R( AXID, 'Base', BASE, STATUS )
      CALL ADI_CGET0R( AXID, 'Scale', SCALE, STATUS )
      CALL ADI_CGET0R( AXID, 'Tor', TOR, STATUS )

*  Release the axis
      CALL ADI_ERASE( AXID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF0_GETAXVAL', STATUS )
      END IF

      END
