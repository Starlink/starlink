      SUBROUTINE PSF1_WFC_DAT( PSID, X0, Y0, QX, QY, DX, DY,
     :                           INTEG, NX, NY, ARRAY, STATUS )
*+
*  Name:
*     PSF1_WFC_DAT

*  Purpose:
*     Return survey WFC psf probability

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF1_WFC_DAT( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
*                          NY, ARRAY, STATUS )

*  Description:
*     Returns integrated psf probability for a NX * NY 2-D detector patch
*     whose position is specified by X0,Y0,QX,QY due to a source at
*     position X0,Y0.

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier of psf storage object
*     X0 = REAL (given)
*        X detector position (radians)
*     Y0 = REAL (given)
*        Y detector position (radians)
*     QX = REAL (given)
*        X offset from psf centre to centre of ARRAY (radians)
*     QY = REAL (given)
*        Y offset from psf centre to centre of ARRAY (radians)
*     DX = REAL (given)
*        Size of ARRAY pixels in X axis (radians)
*     DY = REAL (given)
*        Size of ARRAY pixels in Y axis (radians)
*     INTEG = LOGICAL (given)
*        Return integrated probability (ie. normalised to unity if ARRAY
*        was sufficiently large)
*     NX = INTEGER (given)
*        X dimension of ARRAY
*     NY = INTEGER (given)
*        Y dimension of ARRAY
*     ARRAY[NX,NY] = REAL (returned)
*        Integrated psf probability
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
*     10 Jul 1989 (DJA):
*        Original version
*     23 Apr 1990 (DJA):
*        Supplies correct energy derived from filter ID
*     23 May 1990 (DJA):
*        Uses new CAL system
*      2 Feb 1993 (DJA):
*        Sign of QX corrected
*      2 May 1996 (DJA):
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
      REAL                     DX, DY, X0, Y0, QX, QY
      INTEGER                  PSID, NX, NY
      LOGICAL                  INTEG

*  Arguments Returned:
      REAL                     ARRAY(NX,NY)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      DOUBLE PRECISION		DMJD			! MJD

      REAL			ENER			! Energy
      REAL			IRIS			! Iris for integration
      REAL                      LDX, LQX                ! Local copies for CAL

      INTEGER			FID			! Filter id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  CAL doesn't handle coordinates as you'd expect!
      LDX = ABS(DX)
      LQX = ABS(QX)

*  Extract data need by CAL
      CALL PSF0_GETID0D( PSID, 'MJD', DMJD, STATUS )
      CALL PSF0_GETID0I( PSID, 'Filter', FID, STATUS )
      CALL PSF0_GETID0R( PSID, 'Iris', IRIS, STATUS )
      CALL PSF0_GETID0R( PSID, 'Energy', ENER, STATUS )

*  Ignore integration flag
      CALL CAL_PSFT2D_SUR( DMJD, FID, ENER, .FALSE., 0.0, IRIS, LDX, NX,
     :                     ABS(DY), NY, QX*(DX/LDX), QY, ARRAY, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF1_WFC_DAT', STATUS )
      END IF

      END
