      SUBROUTINE WCI1_READ( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     WCI1_READ

*  Purpose:
*     Read in the WCS info from an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI1_READ( NARG, ARGS, OARG, STATUS )

*  Description:
*     Constructs the data objects required by WCI from the supplied HDS
*     dataset. This may be any Starlink NDF, but if an ASTERIX dataset
*     is supplied then additional astrometic information can be extracted.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     Creates a PSF_SLOT property on the property list of the first
*     argument if one is not already present.

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     SLA:
*        SLA_EPJ	- MJD to Julian epoch conversion

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants
      INCLUDE 'DAT_PAR'					! HDS constants
      INCLUDE 'WCI_PAR'					! WCI constants

*  Arguments Given:
      INTEGER			NARG			! # arguments
      INTEGER			ARGS(*)			! Method arguments

*  Arguments Returned:
      INTEGER			OARG			! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			SLA_EPJ
        DOUBLE PRECISION	SLA_EPJ

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	HLOC			! Object header
      CHARACTER*80		LABEL			! X,Y axis labels
      CHARACTER*3		PRJ			! Projection name
      CHARACTER*3		SYS			! Coord system name
      CHARACTER*40		UNITS(2)		! X,Y axis units

      DOUBLE PRECISION		BTAI			! Value of BASE_TAI
      DOUBLE PRECISION		EPOCH			! Epoch
      DOUBLE PRECISION		MJD			! Observation time
      DOUBLE PRECISION		NPOINT(2)		! Nominal RA, DEC
      DOUBLE PRECISION		PA			! Position angle
      DOUBLE PRECISION		SPOINT(2)		! RA, DEC

      REAL			BASE(2), SCALE(2)	! Axis values
      REAL			EQNX			! Equinox
      REAL			TOR			! Radian conversion

      INTEGER			DIMS(2)			! Axis dimensions
      INTEGER			IMJD			! Value of BASE_MJD
      INTEGER			IPSF			! Psf system handle
      INTEGER			PIXID			! Pixellation object
      INTEGER			PRJID			! Projection object
      INTEGER			PTR(2)			! Axis data pointers
      INTEGER			SYSID			! CoordSystem object
      INTEGER			X_AX,Y_AX,E_AX,T_AX	! Axis numbers

      LOGICAL			EQOK, EPOK		! Equinox & epoch ok?
      LOGICAL			FRAOK, FDECOK		! Nominal pointing ok?
      LOGICAL			HASPIX			! Spatial axes exist?
      LOGICAL			RAOK, DECOK, PAOK       ! Found ok flags
      LOGICAL			REG(2)			! Axes regular
      LOGICAL			PRJOK, SYSOK		! Projection/system ok?
      LOGICAL			TAIOK			! BASE_TAI found?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise flags
      EPOK = .FALSE.
      EQOK = .FALSE.
      DECOK = .FALSE.
      RAOK = .FALSE.
      PAOK = .FALSE.
      PRJOK = .FALSE.
      SYSOK = .FALSE.

*  Introduce the locator to the PSF system
      CALL PSF_INTRO( ARGS(1), IPSF, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        HASPIX = .TRUE.
      ELSE
        CALL ERR_ANNUL( STATUS )
        HASPIX = .FALSE.
      END IF

*  Extract axis info
      IF ( HASPIX ) THEN
        CALL PSF_QAXES( IPSF, X_AX, Y_AX, E_AX, T_AX, STATUS )
        IF ( (X_AX .LT. 1) .OR. (Y_AX.LT.1) ) THEN
          HASPIX = .FALSE.
        ELSE
          CALL PSF_QAXIS( IPSF, X_AX, DIMS(1), REG(1), PTR(1), BASE(1),
     :                SCALE(1), LABEL, UNITS(1), TOR, STATUS )
          BASE(1) = BASE(1) / TOR
          SCALE(1) = SCALE(1) / TOR
          CALL PSF_QAXIS( IPSF, Y_AX, DIMS(2), REG(2), PTR(2), BASE(2),
     :                SCALE(2), LABEL, UNITS(2), TOR, STATUS )
          BASE(2) = BASE(2) / TOR
          SCALE(2) = SCALE(2) / TOR
        END IF
      END IF

*  Look for ASTERIX header data
      CALL ADI1_LOCHEAD( ARGS(1), .FALSE., HLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Extract data from header if present
*     Pointing and orientation
        CALL ADI1_CGET0D( HLOC, 'AXIS_RA', RAOK, SPOINT(1), STATUS )
        CALL ADI1_CGET0D( HLOC, 'AXIS_DEC', DECOK, SPOINT(2), STATUS )
        CALL ADI1_CGET0D( HLOC, 'FIELD_RA', FRAOK, NPOINT(1), STATUS )
        CALL ADI1_CGET0D( HLOC, 'FIELD_DEC', FDECOK, NPOINT(2), STATUS )
        CALL ADI1_CGET0D( HLOC, 'POSITION_ANGLE', PAOK, PA, STATUS )
        IF ( .NOT. PAOK ) THEN
          PA = 0D0
        END IF

*     Equinox
        CALL ADI1_CGET0R( HLOC, 'EQUINOX', EQOK, EQNX, STATUS )

*     Epoch of observation. Composed of the BASE_MJD and BASE_TAI header
*     components
        CALL ADI1_CGET0I( HLOC, 'BASE_MJD', EPOK, IMJD, STATUS )
        IF ( EPOK ) THEN
          CALL ADI1_CGET0D( HLOC, 'BASE_TAI', TAIOK, BTAI, STATUS )
          IF ( TAIOK ) THEN
            MJD = DBLE(IMJD) + BTAI / 86400D0
          ELSE
            MJD = DBLE(IMJD)
          END IF
          EPOCH = SLA_EPJ( MJD )
        END IF

*     ASTERIX is always TAN projection
        PRJ = 'TAN'
        PRJOK = .TRUE.

*     ASTERIX doesn't specify the system in the header, but the only FK4
*      data is the SL2 stuff where the equinox was 1950
        IF ( EQOK ) THEN
          IF ( NINT(EQNX) .EQ. 1950 ) THEN
            SYS = 'FK4'
          ELSE
            SYS = 'FK5'
          END IF
          SYSOK = .TRUE.
        END IF

      ELSE
        CALL ERR_ANNUL( STATUS )

      END IF

*  Create the CoordSystem object. Define suitable defaults for missing data
      IF ( .NOT. SYSOK ) SYS = 'FK5'
      IF ( .NOT. EQOK ) EQNX = 2000.0
      IF ( .NOT. EPOK ) EPOCH = WCI__FLAG
      CALL WCI_NEWSYS( SYS, EQNX, EPOCH, SYSID, STATUS )

*  Create the Projection object, providing defaults
      IF ( .NOT. PRJOK ) PRJ = 'TAN'
      IF ( .NOT. RAOK ) SPOINT(1) = 0D0
      IF ( .NOT. DECOK ) SPOINT(2) = 0D0
      CALL WCI_NEWPRJ( PRJ, 0, 0.0, SPOINT, 180D0, PRJID, STATUS )
      IF ( FRAOK .AND. FDECOK ) THEN
        CALL ADI_CPUT1D( PRJID, 'NPOINT', 2, NPOINT, STATUS )
      END IF

*  Create the pixellation object, providing defaults
      IF ( HASPIX ) THEN
        IF ( .NOT. PAOK ) PA = 0D0
        IF ( REG(1) .AND. REG(2) ) THEN
          CALL WCI_NEWPX( DIMS, BASE, SCALE, UNITS, PA, PIXID, STATUS )
        ELSE
	  PRINT *, 'Irregular axes, not yet!'
        END IF
      END IF

*  Create the output structure
      CALL ADI_NEW0( 'STRUC', OARG, STATUS )
      IF ( HASPIX ) THEN
        CALL ADI_CPUTID( OARG, 'Pix', PIXID, STATUS )
      END IF
      CALL ADI_CPUTID( OARG, 'Proj', PRJID, STATUS )
      CALL ADI_CPUTID( OARG, 'Sys', SYSID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'WCI1_READ', STATUS )
      END IF

      END
