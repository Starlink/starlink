      REAL FUNCTION PSF1_ASCA_E( PSID, STATUS )
*+
*  Name:
*     PSF1_ASCA_E

*  Purpose:
*     Get energy in keV from ASCA psf identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = PSF1_ASCA_E( PSID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier to psf storage object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Returned Value:
*     PSF1_ASCA_E = REAL
*        Psf energy in keV

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
*      1 May 1996 (DJA):
*        Original version.
*     21 Jun 1996 (DJA):
*        Now assumes SIS data grouped by factor 8
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER			PSID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL			GIS_GAIN		! GIS PI gain
        PARAMETER		( GIS_GAIN = 1.07E-2 )	! keV per PI channel
      REAL			SIS_GAIN		! SIS PI gain
        PARAMETER		( SIS_GAIN = 8*3.65E-3 )! keV per PI channel

*  Local Variables:
      CHARACTER*4		INS			! Instrument name

      REAL			CSCALE			! Channel scaling
      REAL			ENERGY			! Psf energy needed

      INTEGER			PHALO, PHAHI		! PHA bounds

      LOGICAL			PHADEF			! PHA band defined?
*.

*  Find PHA bounds
      CALL ADI_CGET0L( PSID, 'PhaDef', PHADEF, STATUS )
      CALL ADI_CGET0I( PSID, 'PhaLo', PHALO, STATUS )
      CALL ADI_CGET0I( PSID, 'PhaHi', PHAHI, STATUS )

*  Get instrument
      CALL PSF0_GETID0C( PSID, 'Instr', INS, STATUS )

*  Find energy in keV, and then energy bin number 1->2, 2->3 etc, and
*  coerce into the range 0 to 10
      IF ( PHADEF ) THEN

*      Get channel scale
        CALL PSF0_GETID0R( PSID, 'ChanScale', CSCALE, STATUS )

*      Convert users PHA to raw PHA
        IF ( INS .EQ. 'SIS' ) THEN
          ENERGY = (REAL(PHALO) + REAL(PHAHI))*SIS_GAIN*CSCALE/2.0
        ELSE
          ENERGY = (REAL(PHALO) + REAL(PHAHI))*GIS_GAIN*CSCALE/2.0
        END IF

      ELSE
        CALL PSF0_GETID0R( PSID, 'Energy', ENERGY, STATUS )

      END IF

*  Set return value
      PSF1_ASCA_E = ENERGY

      END
