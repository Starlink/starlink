      SUBROUTINE PSF1_ASIS_DAT( PSID, X0, Y0, QX, QY, DX, DY,
     :                          INTEG, NX, NY, ARRAY, STATUS )
*+
*  Name:
*     PSF1_ASIS_DAT

*  Purpose:
*     Return 2-D probability array for the ASCA SIS

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF1_ASIS_DAT( PSID, X0, Y0, QX, QY, DX, DY, INTEG, NX,
*                         NY, ARRAY, STATUS )

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
*     15 May 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'PSF_ASCA_CMN'

*  Arguments Given:
      REAL                      DX, DY, X0, Y0, QX, QY
      INTEGER                   PSID, NX, NY
      LOGICAL                   INTEG

*  Arguments Returned:
      REAL                      ARRAY(NX,NY)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      REAL                      ENERGY                  ! Mean photon energy
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure the psf cube is loaded
      IF ( .NOT. AS_SIS_LOAD ) THEN

*    Load grid
        CALL PSF0_LDGRID( 'asca_sis_psfs', AS_SIS_DIMS, AS_SIS_NDIM,
     :                    AS_SIS_DX, AS_SIS_XYSAM, AS_SYS_RPTR,
     :                    AS_SIS_EPTR, AS_SIS_FLAGS, AS_SIS_DPTR,
     :                    STATUS )

*    Mark as loaded
        AS_SIS_LOAD = (STATUS.EQ.SAI__OK)

      END IF

*  Get the energy to use
      ENERGY = PSF1_ASCA_E( PSID, STATUS )

*  Use the grid to get the psf
      CALL PSF0_USEGRID( AS_SIS_DIMS, AS_SIS_NDIM, AS_SIS_DX,
     :                   AS_SIS_XYSAM, AS_SYS_RPTR, AS_SIS_EPTR,
     :                   AS_SIS_FLAGS, AS_SIS_DPTR,
     :                   PSID, ENERGY, X0, Y0, QX, QY, DX, DY,
     :                   NX, NY, ARRAY, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF1_ASIS_DAT', STATUS )
      END IF

      END
