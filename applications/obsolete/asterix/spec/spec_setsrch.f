*+  SPEC_SETSRCH - Checks file to see if it is a SPECTRAL_SET with response
      SUBROUTINE SPEC_SETSRCH( FID, SET, STATUS )
*    Description :
*     Searches the object located by LOC and checks for spectral set
*     characteristics. This requires that its TYPE should be 'SPECTRAL_SET'
*     or that the following conditions are all met:
*      (a) The dataset should be 2D
*      (b) The first axis should be PULSE_* or CHANNEL or ENERGY
*      (c) The ENERGY_RESP should be an array of structures equal in
*          length to the non-spectral dimension of the data array
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     Trevor Ponman (BHVAD::TJP)
*
*    History :
*
*     18 May 90 : Original (TJP)
*     25 Jan 94 : Error handling corrected (DJA)
*     21 Nov 95 : ADI port (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Import :
*
      INTEGER			FID
*
*    Export :
*
	LOGICAL SET			! True if SPECTRAL_SET
*
*    Status :
*
	INTEGER STATUS
*    Local variables :
	LOGICAL OK			! Component present
	INTEGER NDIM			! Number of axes
	INTEGER DIMS(ADI__MXDIM)	! Axis dimensions
	INTEGER AXSPEC			! Number of the spectral axis (1 or 2)
	INTEGER AXOTHER			! Number of non-spectral axis
	INTEGER DIMOTHER		! Size of non-spectral axis
*-

*  Status check
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      SET = .FALSE.

*  Check dimensionality
      CALL BDI_CHK( FID, 'Data', OK, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
	CALL ERR_ANNUL( STATUS )
	GOTO 99
      END IF
      CALL BDI_GETSHP( FID, 2, DIMS, NDIM, STATUS )
      IF ( NDIM .NE. 2 ) GOTO 99

*  Look for spectral axis
      CALL BDI0_FNDAXC( FID, 'E', AXSPEC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        GOTO 99
      END IF

*  Non-spectral axis (must be first at present)
      IF ( AXSPEC .EQ. 1 ) THEN
	AXOTHER = 2
      ELSE
	STATUS = SAI__ERROR
        CALL ERR_REP('AXORD','Spectral dimension must be first',
     :    STATUS)
	GOTO 99
      END IF
      DIMOTHER = DIMS(AXOTHER)

      SET = .TRUE.

*  Check that ENERGY_RESP is same size as non-spectral axis
c	CALL DAT_FIND(LOC,'MORE',LOC1,STATUS)
c	CALL DAT_FIND(LOC1,'ASTERIX',LOC2,STATUS)
c	CALL CMP_SHAPE(LOC2,'ENERGY_RESP',DAT__MXDIM,DIMS,NDIM,STATUS)
c	IF(STATUS.NE.SAI__OK)THEN
c	  CALL ERR_ANNUL(STATUS)	! Assume NOT a spectral set
c	  GO TO 99
c	ENDIF
c	IF((NDIM.EQ.1).AND.(DIMS(1).EQ.DIMOTHER))THEN
c	  SET=.TRUE.
c	ENDIF
c	CALL DAT_ANNUL(LOC1,STATUS)
c	CALL DAT_ANNUL(LOC2,STATUS)

*  Exit
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SPEC_SETSRCH', STATUS )
      END IF

      END
