      SUBROUTINE WCI2_READ( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     WCI2_READ

*  Purpose:
*     Read in the WCS info from a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI2_READ( NARG, ARGS, OARG, STATUS )

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
      INCLUDE 'WCI_PAR'					! WCI constants

*  Arguments Given:
      INTEGER			NARG, ARGS(*)

*  Arguments Returned:
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			SLA_EPJ
        DOUBLE PRECISION	SLA_EPJ

*  Local Constants:
      INTEGER			MAXPP			! Max # proj params
        PARAMETER		( MAXPP = 10 )

*  Local Variables:
      CHARACTER*40		CTYPE			! CTYPE value
      CHARACTER*80		LABEL			! X,Y axis labels
      CHARACTER*3		PRJ			! Projection name
      CHARACTER*3		RADECSYS		! Coord system name
      CHARACTER*3		SYS			! Coord system name
      CHARACTER*40		UNITS(2)		! X,Y axis units

      DOUBLE PRECISION		BTAI			! Value of BASE_TAI
      DOUBLE PRECISION		EPOCH			! Epoch
      DOUBLE PRECISION		LONGPOLE		!
      DOUBLE PRECISION		MJD			! Observation time
      DOUBLE PRECISION		PA			! Position angle
      DOUBLE PRECISION		SPOINT(2)		! RA, DEC

      REAL			BASE(2), SCALE(2)	! Axis values
      REAL			EQNX			! Equinox
      REAL			PPARS(MAXPP)		! Projections params
      REAL			TOR			! Radian conversion

      INTEGER			DIMS(2)			! Axis dimensions
      INTEGER			IMJD			! Value of BASE_MJD
      INTEGER			IP			! Loop over proj params
      INTEGER			IPSF			! Psf system handle
      INTEGER			NPRJP			! # projection pars
      INTEGER			PHDU			! Primary HDU
      INTEGER			PIXID			! Pixellation object
      INTEGER			PRJID			! Projection object
      INTEGER			PTR(2)			! Axis data pointers
      INTEGER			SYSID			! CoordSystem object
      INTEGER			X_AX,Y_AX,E_AX,T_AX	! Axis numbers

      LOGICAL			EQOK, EPOK		! Equinox & epoch ok?
      LOGICAL			HASPIX			! Spatial axes exist?
      LOGICAL			LPOK			! Longpole found?
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
      LPOK = .FALSE.
      RAOK = .FALSE.
      PAOK = .FALSE.
      PRJOK = .FALSE.
      SYS = '   '
      SYSOK = .FALSE.

*  Introduce the identifier to the PSF system
      CALL PSF_INTRO( ARGS(1), IPSF, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        HASPIX = .TRUE.
      ELSE
        CALL ERR_ANNUL( STATUS )
        HASPIX = .FALSE.
      END IF

*  Locate main HDU
      CALL ADI2_FNDHDU( ARGS(2), ' ', PHDU, STATUS )

*  Look for the RADECSYS keyword
      CALL ADI2_HGKYC( PHDU, 'RADECSYS', RADECSYS, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        IF ( RADECSYS .EQ. 'FK4' ) THEN
          SYS = 'FK4'
        ELSE IF ( RADECSYS .EQ. 'FK5' ) THEN
          SYS = 'FK5'
        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'SYS', RADECSYS )
          CALL ERR_REP( ' ', 'Unrecognised RADECSYS value', STATUS )
          CALL ERR_ANNUL( STATUS )
        END IF
        IF ( SYS(1:1) .NE. ' ' ) SYSOK = .TRUE.

      ELSE
        RADECSYS = ' '
        CALL ERR_ANNUL( STATUS )

      END IF

*  Epoch of observation?
      CALL ADI2_HGKYD( PHDU, 'MJD-OBS', MJD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        EPOCH = SLA_EPJ( MJD )
        EPOK = .TRUE.
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

*  Equinox of coordinates
      CALL ADI2_HGKYR( PHDU, 'EQUINOX', EQNX, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        EQOK = .TRUE.
        IF ( .NOT. EPOK ) THEN
          EPOCH = DBLE(EQNX)
          EPOK = .TRUE.
        END IF
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

*  Look for old EPOCH keyword if epoch not defined
      IF ( .NOT. EPOK ) THEN
        CALL ADI2_HGKYD( PHDU, 'EPOCH', EPOCH, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          EPOK = .TRUE.
        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF
      END IF

*  Look for projection params
      NPRJP = 0
      IP = 1
      DO WHILE ( (STATUS .EQ. SAI__OK) .AND. (IP.LE.MAXPP) )
        CALL ADI2_HGKYIR( PHDU, 'PROJP', IP, PPARS(IP), STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          NPRJP = NPRJP + 1
          IP = IP + 1
        END IF
      END DO
      CALL ERR_ANNUL( STATUS )

*  Look at CTYPEi to determinate projection, and possibly system too
      CALL ADI2_HGKYC( PHDU, 'CTYPE1', CTYPE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Projection defined?
        IF ( CTYPE(6:) .GT. ' ' ) THEN
          PRJ = CTYPE(6:)
          PRJOK = .TRUE.
        END IF

*    Determine system from first 4 characters?
        IF ( .NOT. SYSOK ) THEN
          IF ( CTYPE(1:2) .EQ. 'RA' ) THEN
            IF ( EPOK ) THEN
              IF ( EPOCH .GT. 1984.0 ) THEN
                SYS = 'FK5'
              ELSE
                SYS = 'FK4'
              END IF
            END IF
          ELSE IF ( CTYPE(1:4) .EQ. 'ELON' ) THEN
            SYS = 'ECL'
          ELSE IF ( CTYPE(1:4) .EQ. 'GLON' ) THEN
            SYS = 'GAL'
          END IF
          IF ( SYS .GT. ' ' ) SYSOK = .TRUE.
        END IF
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

      CALL ADI2_HGKYC( PHDU, 'CTYPE2', CTYPE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Projection defined?
        IF ( CTYPE(6:) .GT. ' ' ) THEN
          PRJ = CTYPE(6:)
          PRJOK = .TRUE.
        END IF

*    Determine system from first 4 characters?
        IF ( .NOT. SYSOK ) THEN
          IF ( CTYPE(1:3) .EQ. 'DEC' ) THEN
            IF ( EPOK ) THEN
              IF ( EPOCH .GT. 1984.0 ) THEN
                SYS = 'FK5'
              ELSE
                SYS = 'FK4'
              END IF
            END IF
          ELSE IF ( CTYPE(1:4) .EQ. 'ELAT' ) THEN
            SYS = 'ECL'
          ELSE IF ( CTYPE(1:4) .EQ. 'GLAT' ) THEN
            SYS = 'GAL'
          END IF
          IF ( SYS .GT. ' ' ) SYSOK = .TRUE.
        END IF
      ELSE
        CALL ERR_ANNUL( STATUS )

      END IF

*  Extract axis info
      CALL ADI2_HGKYD( PHDU, 'CRPIX1', SPOINT(1), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        RAOK = .TRUE.
      END IF
      CALL ADI2_HGKYD( PHDU, 'CRPIX2', SPOINT(2), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        DECOK = .TRUE.
      END IF

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

*  Try to get position angle
      CALL ADI2_HGKYD( PHDU, 'CROTA2', PA, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL ADI2_HGKYD( PHDU, 'DROLLANG', PA, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
        ELSE
          PAOK = .TRUE.
        END IF

      ELSE
        PAOK = .TRUE.
      END IF

*  Create the CoordSystem object. Define suitable defaults for missing data
      IF ( .NOT. SYSOK ) SYS = 'FK5'
      IF ( .NOT. EQOK ) EQNX = 2000.0
      IF ( .NOT. EPOK ) EPOCH = WCI__FLAG
      CALL WCI_NEWSYS( SYS, EQNX, EPOCH, SYSID, STATUS )

*  Create the Projection object, providing defaults
      IF ( .NOT. PRJOK ) PRJ = 'CAR'
      IF ( .NOT. RAOK ) SPOINT(1) = 0D0
      IF ( .NOT. DECOK ) SPOINT(2) = 0D0
      IF ( .NOT. LPOK ) LONGPOLE = 180D0
      CALL WCI_NEWPRJ( PRJ, NPRJP, PPARS, SPOINT, LONGPOLE,
     :                 PRJID, STATUS )

*  Create the pixellation object, providing defaults
      IF ( HASPIX ) THEN
        IF ( .NOT. PAOK ) PA = 0D0
        CALL WCI_NEWPX( DIMS, BASE, SCALE, UNITS, PA, PIXID, STATUS )
      END IF

*  Create the output structure
      CALL ADI_NEW0( 'STRUC', OARG, STATUS )
      IF ( HASPIX ) THEN
        CALL ADI_CPUTID( OARG, 'Pix', PIXID, STATUS )
      END IF
      CALL ADI_CPUTID( OARG, 'Proj', PRJID, STATUS )
      CALL ADI_CPUTID( OARG, 'Sys', SYSID, STATUS )
	call adi_print(oarg,status)
*  Release primary HDU
      CALL ADI_ERASE( PHDU, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'WCI2_READ', STATUS )
      END IF

      END
