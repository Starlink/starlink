      SUBROUTINE PSF0_PUTAX( PSID, AX, OK, DIM, REG, APTR, BASE,
     :                       SCALE, TOR, LABEL, UNITS, STATUS )
*+
*  Name:
*     PSF0_PUTAX

*  Purpose:
*     Store axis info in a psf storage object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF0_PUTAX( PSID, AX, OK, DIM, REG, APTR, BASE, SCALE, TOR
*                      LABEL, UNITS, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        Psf storage identifier
*     AX = INTEGER (given)
*        Axis number
*     OK = LOGICAL (given)
*        Axis data ok?
*     DIM = INTEGER (given)
*        Axis size
*     REG = LOGICAL (given)
*        Axis values regularly spaced?
*     APTR = INTEGER (given)
*        Axis data pointer if REG false
*     BASE = REAL (given)
*        Axis data base
*     SCALE = REAL (given)
*        Axis data scale
*     TOR = REAL (given)
*        Conversion factor to radians
*     LABEL = CHARACTER*(*) (given)
*        Axis data label
*     UNITS = CHARACTER*(*) (given)
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
      INTEGER			PSID, AX, DIM, APTR
      LOGICAL			OK, REG
      REAL			BASE, SCALE, TOR
      CHARACTER*(*)		LABEL, UNITS

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			AXID			! Axis identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the AX'th axis
      CALL ADI_CCELL( PSID, 'Axes', 1, AX, AXID, STATUS )

*  Store the stuff
      CALL ADI_CPUT0L( AXID, 'Ok', OK, STATUS )
      CALL ADI_CPUT0I( AXID, 'Dim', DIM, STATUS )
      CALL ADI_CPUT0L( AXID, 'Reg', REG, STATUS )
      CALL ADI_CPUT0I( AXID, 'Ptr', APTR, STATUS )
      CALL ADI_CPUT0R( AXID, 'Base', BASE, STATUS )
      CALL ADI_CPUT0R( AXID, 'Dr', SCALE*TOR, STATUS )
      CALL ADI_CPUT0R( AXID, 'Scale', SCALE, STATUS )
      CALL ADI_CPUT0R( AXID, 'Tor', TOR, STATUS )
      CALL ADI_CPUT0C( AXID, 'Label', LABEL, STATUS )
      CALL ADI_CPUT0C( AXID, 'Units', UNITS, STATUS )

*  Free the axis
      CALL ADI_ERASE( AXID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PSF0_PUTAX', STATUS )

      END
