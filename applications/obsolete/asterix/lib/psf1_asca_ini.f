      SUBROUTINE PSF1_ASCA_INI( PSID, STATUS )
*+
*  Name:
*     PSF1_ASCA_INI

*  Purpose:
*     Initialise an ASCA psf

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF1_ASCA_INI( PSID, STATUS )

*  Description:
*

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier of psf storage object. The data held by the object
*        is altered
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
*     12 Jan 1994 (DJA):
*        Original version.
*      7 May 1996 (DJA):
*        New header
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'

*  Global Variables:
      INCLUDE 'PSF_ASCA_CMN'

*  Arguments Given:
      INTEGER                   PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			CHR_SIMLR
        LOGICAL                 CHR_SIMLR
      EXTERNAL			PSF1_ASCA_DAT
      EXTERNAL			PSF1_ASCA_EPF
      EXTERNAL			PSF1_ASCA_HNT

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ILOC
      CHARACTER*3	    	INS			! Instrument name
      CHARACTER*20            	MASK              	! Mask name

      REAL			CSCALE			! Channel scale
      REAL			RCLO,RCHI,CLO,CHI
      REAL 			ENERGY			! Mean photon energy

      INTEGER			ABPTR			! Axis bounds array
      INTEGER			DIMS(ADI__MXDIM), NDIM	! Dataset dimensions
      INTEGER			FID			! File identifier
      INTEGER			I,J			!
      INTEGER			SLOT			! Psf slot number
      INTEGER			X_AX,Y_AX,E_AX,T_AX

      LOGICAL			PHADEF			! PHA band defined?

*  Local Data:
      LOGICAL			FIRST
	SAVE			FIRST
      DATA			FIRST/.TRUE./
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Clear pointer arrays for ASCA psfs
      IF ( FIRST ) THEN
        DO I = 1, AS_NSP
          DO J = 1,  AS_NE
            AS_GPTR(J,I) = 0
            AS_SPTR(J,I) = 0
          END DO
        END DO
        FIRST = .FALSE.
      END IF

*  Get directory containing FTOOLS calibrations
      CALL PSX_GETENV( 'AST_FTOOLS_CALDB', AS_CALDB, STATUS )
      IF ( AS_CALDB(1:7) .EQ. 'unknown' ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FTOOLS calibrations directory has not '/
     :   /'been set up; see "asthelp 5 6 FTOOLS" for more', STATUS )
        GOTO 99
      ELSE
        AS_CALDBL = CHR_LEN( AS_CALDB )
      END IF

*  Get mask name
      CALL USI_PROMT( 'MASK', 'ASCA detector (GIS or SIS)', STATUS )
      CALL USI_DEF0C( 'MASK', 'GIS', STATUS )
 10   CALL USI_GET0C( 'MASK', MASK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Validate choice
      IF ( CHR_SIMLR(MASK,'GIS') ) THEN
        INS = 'GIS'

      ELSE IF ( CHR_SIMLR(MASK,'SIS') ) THEN
        INS = 'SIS'

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'MASK', MASK )
        CALL ERR_REP( ' ', 'Invalid ASCA instrument name'/
     :                /' /^MASK/', STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL PSF0_SETID0C( PSID, 'Instr', INS, STATUS )

*  User is supplying PHA bounds
      CALL ADI_CGET0L( PSID, 'PhaDef', PHADEF, STATUS )
      IF ( PHADEF ) THEN

*    Locate INSTRUMENT box
        CALL ADI_CGET0I( PSID, 'FileID', FID, STATUS )
        CALL ADI1_LOCINSTR( FID, .FALSE., ILOC, STATUS )
        CALL CMP_GET0R( ILOC, 'RCHANLO', RCLO, STATUS )
        CALL CMP_GET0R( ILOC, 'RCHANHI', RCHI, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN

          CALL ERR_ANNUL( STATUS )
          RCLO = 0.0
          IF ( INS .EQ. 'SIS' ) THEN
            RCHI = 4095.0
          ELSE
            RCHI = 1023.0
          END IF
          CALL MSG_SETR( 'TOP', RCHI )
          CALL MSG_PRNT( 'Error reading raw channel bounds, '/
     :                 /'assuming 0..^TOP' )
        END IF

*      Get channel axis range
        CALL ADI_CGET0I( PSID, 'Slot', SLOT, STATUS )
        CALL PSF_QAXES( SLOT, X_AX, Y_AX, E_AX, T_AX, STATUS )
        IF ( E_AX .GT. 0 ) THEN
          CALL BDI_GETSHP( FID, ADI__MXDIM, DIMS, NDIM, STATUS )
          CALL BDI_AXMAPR( FID, E_AX, 'Bounds', 'READ', ABPTR, STATUS )
          CALL ARR_ELEM1R( ABPTR, 2*DIMS(E_AX), 1, CLO, STATUS )
          CALL ARR_ELEM1R( ABPTR, 2*DIMS(E_AX), 2*DIMS(E_AX),
     :                     CHI, STATUS )
          CALL BDI_AXUNMAP( FID, E_AX, 'Bounds', ABPTR, STATUS )
          CSCALE = REAL(RCHI-RCLO)/REAL(CHI-CLO)
          CALL MSG_SETR( 'SC', CSCALE )
	  CALL MSG_PRNT( 'User to RAW channel scaling = ^SC' )
        ELSE
          CSCALE = 1.0
        END IF
        CALL PSF0_SETID0R( PSID, 'ChanScale', CSCALE, STATUS )

*    Get a mean photon energy
      ELSE
        CALL USI_PROMT( 'AUX', 'Mean photon energy in keV', STATUS )
        CALL USI_GET0R( 'AUX', ENERGY, STATUS )
        CALL PSF0_SETID0R( PSID, 'Energy', ENERGY, STATUS )

      END IF

*  Store methods
      CALL PSF0_SETRTN( PSID, 'Data', PSF1_ASCA_DAT, STATUS )
      CALL PSF0_SETRTN( PSID, 'Eprofile', PSF1_ASCA_EPF, STATUS )
      CALL PSF0_SETRTN( PSID, 'Hint', PSF1_ASCA_HNT, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF1_ASCA_INI', STATUS )
      END IF

      END
