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
      CHARACTER*72		CMNT			! Keyword comment string
      CHARACTER*20		DECSGN			! Sign of declination

      DOUBLE PRECISION		EPOCH			! Epoch
      DOUBLE PRECISION		LONGPOLE		!
      DOUBLE PRECISION		MJD			! Observation time
      DOUBLE PRECISION		PA			! Position angle
      DOUBLE PRECISION		SPOINT(2)		! RA, DEC

      REAL			BASE(2), SCALE(2)	! Axis values
      REAL			EQNX			! Equinox
      REAL			PPARS(MAXPP)		! Projections params
      REAL			TOR			! Radian conversion
      REAL			SECS			! RA/DEC componemts

      INTEGER			DEGS, MINS, HOURS	! RA/DEC componemts
      INTEGER			DIMS(2)			! Axis dimensions
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
      IF ( ARGS(1) .EQ. ADI__NULLID ) THEN
        CALL PSF_INTRO( ARGS(2), IPSF, STATUS )
      ELSE
        CALL PSF_INTRO( ARGS(1), IPSF, STATUS )
      END IF
      IF ( STATUS .EQ. SAI__OK ) THEN
        HASPIX = .TRUE.
      ELSE
        CALL ERR_ANNUL( STATUS )
        HASPIX = .FALSE.
      END IF

*  Locate main HDU
      CALL ADI2_FNDHDU( ARGS(2), ' ', .FALSE., PHDU, STATUS )

*  Look for the RADECSYS keyword
      CALL ADI2_HGKYC( PHDU, 'RADECSYS', RADECSYS, CMNT, STATUS )
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
      CALL ADI2_HGKYD( PHDU, 'MJD-OBS', MJD, CMNT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        EPOCH = SLA_EPJ( MJD )
        EPOK = .TRUE.
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

*  Equinox of coordinates
      CALL ADI2_HGKYR( PHDU, 'EQUINOX', EQNX, CMNT, STATUS )
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
        CALL ADI2_HGKYD( PHDU, 'EPOCH', EPOCH, CMNT, STATUS )
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
        CALL ADI2_HGKYIR( PHDU, 'PROJP', IP, PPARS(IP), CMNT, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          NPRJP = NPRJP + 1
          IP = IP + 1
        END IF
      END DO
      CALL ERR_ANNUL( STATUS )

*  Look at CTYPE1 to determinate projection, and possibly system too
      CALL ADI2_HGKYC( PHDU, 'CTYPE1', CTYPE, CMNT, STATUS )
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

      CALL ADI2_HGKYC( PHDU, 'CTYPE2', CTYPE, CMNT, STATUS )
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
*  Right ascension
      CALL ADI2_HGKYD( PHDU, 'CRVAL1', SPOINT(1), CMNT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL ADI2_HGKYI( PHDU, 'PLTRAH', HOURS, CMNT, STATUS )
        CALL ADI2_HGKYI( PHDU, 'PLTRAM', MINS, CMNT, STATUS )
        CALL ADI2_HGKYR( PHDU, 'PLTRAS', SECS, CMNT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
        ELSE
          SPOINT(1) = (HOURS*15.0D0) + (MINS/60.0D0) + (SECS/3600.0D0)
          RAOK = .TRUE.
        END IF
      ELSE
        RAOK = .TRUE.
      END IF

*  Declination
      CALL ADI2_HGKYD( PHDU, 'CRVAL2', SPOINT(2), CMNT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL ADI2_HGKYC( PHDU, 'PLTDECSN', DECSGN, CMNT, STATUS )
        CALL ADI2_HGKYI( PHDU, 'PLTDECD', DEGS, CMNT, STATUS )
        CALL ADI2_HGKYI( PHDU, 'PLTDECM', MINS, CMNT, STATUS )
        CALL ADI2_HGKYR( PHDU, 'PLTDECS', SECS, CMNT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
        ELSE
          SPOINT(2) = (DEGS/1.0D0) + (MINS/60.0D0) + (SECS/3600.0D0)
          IF ( DECSGN(1:1) .EQ. '-' ) SPOINT(2) = -1.0D0 * SPOINT(2)
          DECOK = .TRUE.
        END IF
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
      CALL ADI2_HGKYD( PHDU, 'CROTA2', PA, CMNT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        CALL ADI2_HGKYD( PHDU, 'DROLLANG', PA, CMNT, STATUS )
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
        STATUS = SAI__OK
      END IF

*  Create the output structure
      CALL ADI_NEW0( 'STRUC', OARG, STATUS )
      IF ( HASPIX ) THEN
        CALL ADI_CPUTID( OARG, 'Pix', PIXID, STATUS )
      END IF
      CALL ADI_CPUTID( OARG, 'Proj', PRJID, STATUS )
      CALL ADI_CPUTID( OARG, 'Sys', SYSID, STATUS )

*  Release primary HDU
      CALL ADI_ERASE( PHDU, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'WCI2_READ', STATUS )
      END IF

      END
