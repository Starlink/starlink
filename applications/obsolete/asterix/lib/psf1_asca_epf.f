      SUBROUTINE PSF1_ASCA_EPF( PSID, X0, Y0, NEEF, EEFS, RADII, STATUS )
*+
*  Name:
*     PSF1_ASCA_EPF

*  Purpose:
*     Return psf radii at specified EEF values at (X0,Y0) on detector

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF1_ASCA_EPF( PSID, X0, Y0, NEEF, EEFS, RADII, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        Psf storage identifier
*     X0 = REAL (given)
*        X position on detector (radians)
*     Y0 = REAL (given)
*        Y position on detector (radians)
*     NEEF = INTEGER (given)
*        Number of radii required
*     EEFS[NEEF] = REAL (given)
*        EEF values at which radii are required
*     RADII[NEEF] = REAL (returned)
*        Radii in radians
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
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 May 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MATH_PAR'

*  Arguments Given:
      INTEGER			PSID, NEEF
      REAL			X0, Y0, EEFS(NEEF)

*  Arguments Returned:
      REAL			RADII(NEEF)

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			PSF1_ASCA_EEF
        REAL			PSF1_ASCA_EEF
      EXTERNAL			PSF1_ASCA_E
        REAL			PSF1_ASCA_E

*  Local Constants:
      REAL			AZI			! Azimuth
        PARAMETER		( AZI = 0.0 )

      REAL			MAXR			! Max radius (mm)
        PARAMETER		( MAXR = 25.0 )

      REAL			MM2AM			! mm -> arcmin
        PARAMETER		( MM2AM = 0.9823 )

*  Local Variables:
      REAL			E			! Energy in keV
      REAL			EEFNORM			! Normalising value
      REAL			ET			! Test EEF value
      REAL			LO_R, HI_R, MID_R	! Test radii (mm)
      REAL			OFF			! Off axis angle (arcmin)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract energy from psf store
      E = PSF1_ASCA_E( PSID, STATUS )

*  Find off-axis angle in arcmin
      OFF = SQRT( X0**2 + Y0**2 ) * MATH__RTOD * 60.0

*  Find EEF at maximum radius
      EEFNORM = PSF1_ASCA_EEF( E, MAXR, OFF, AZI )

*  Loop over required EEF values
      DO I = 1, NEEF

*    Target EEF to find
        TEEF = EEFS(I) * EEFNORM

*    Binary split to find radius
	LO_R = 0.0
	HI_R = MAXR
        DO WHILE ( ABS(HI_R-LO_R) .GT. 0.001 * HI_R )
	  MID_R = (LO_R+HI_R)/2.0
	  ET = PSF1_ASCA_EEF( E, (LO_R+HI_R)/2.0, OFF, 0.0 )
	  IF ( ET .GT. TEEF ) THEN
	    HI_R = MID_R
	  ELSE
	    LO_R = MID_R
	  END IF
	END DO

*    Set radius
        RADII(I) = MM2AM*MATH__DTOR*60.0*(LO_R+HI_R)/2.0

*  Next EEF value
      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF1_ASCA_EPF', STATUS )
      END IF

      END
