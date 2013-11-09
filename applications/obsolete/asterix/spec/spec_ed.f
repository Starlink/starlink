*+ SPEC_ED - Absorption edge
      SUBROUTINE SPEC_ED(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Multiplicative absorption edge above with threshold energy ET
*     i.e. factor
*		EXP(-D*(ET/E)**3)	for E>=ET
*         and          1		for E<ET
*     where
*	D  = PARAM(1)	Optical depth at threshold
*	ET = PARAM(2)	Threshold energy (keV)
*    Method :
*     Quadratic integral approximation used for each bin.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*
*     29 Nov 90 : Original (TJP)
*     25 Feb 93 : Error handling corrected (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	INTEGER NEN			! No of energy channels
	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
	REAL PARAM(3)			! Model parameters
*    Import-Export :
*    Export :
	REAL FLUX(NEN)			! Photon flux (phot/cm**2/s)
*    Status :
	INTEGER STATUS
*    Local variables :
	INTEGER I
	REAL D				! Abs. depth at threshold
	REAL ET				! Threshold energy
	REAL EFAC			! Exponential factor
	REAL E				! Current energy
	REAL E0				! Base energy for channel
	REAL DE				! Channel width
	REAL Y0,Y1,Y2			! Unnormalised function at
					! bottom, middle & top of channel
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check for spot value
	IF(ELBOUND(1).EQ.EUBOUND(1))THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('SPOT','Spot values not supported by SPEC_ routines',
     :    STATUS)
	  GO TO 9000
	ENDIF

* Parameters
	D=PARAM(1)
	ET=PARAM(2)

* Average over each energy channel (trapping case of v. large exponent)
	E0=ELBOUND(1)
	IF(E0.GE.ET)THEN
	  EFAC=MIN(D*(ET/E0)**3,85.0)
	  Y0=EXP(-EFAC)
	ELSE
	  Y0=1
	ENDIF
	DO I=1,NEN
	  DE=EUBOUND(I)-E0
	  E=E0+DE/2
	  IF(E.GE.ET)THEN
	    EFAC=MIN(D*(ET/E)**3,85.0)
	    Y1=EXP(-EFAC)
	  ELSE
	    Y1=1
	  ENDIF
	  E=EUBOUND(I)
	  IF(E.GE.ET)THEN
	    EFAC=MIN(D*(ET/E)**3,85.0)
	    Y2=EXP(-EFAC)
	  ELSE
	    Y2=1
	  ENDIF
	  CALL MATH_INTEGRT(1.0,Y0,Y1,Y2,FLUX(I))
	  E0=E
	  Y0=Y2
	ENDDO

* Exit
 9000 IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SPEC_ED', STATUS )
      END IF

      END
