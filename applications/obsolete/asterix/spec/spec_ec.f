*+ SPEC_EC - Exponential cutoff
      SUBROUTINE SPEC_EC(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Multiplicative exponential cutoff above energy EC
*     i.e. factor
*			EXP(-(E-EC)/EF)    for E>EC
*             and          1               for E<EC
*     where
*	EC = PARAM(1)
*	EF = PARAM(2)
*    Method :
*     Quadratic integral approximation used for each bin.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     29 Nov 90 : Original (TJP
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
	REAL EC				! Cutoff energy
	REAL EF				! E-folding energy
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
	EC=PARAM(1)
	EF=PARAM(2)

* EF=0 case
	IF(EF.EQ.0.0)THEN
	  DO I=1,NEN
	    FLUX(I)=0.0
	  ENDDO
	  GO TO 9000
	ENDIF

* Average over each energy channel
	E0=ELBOUND(1)
	EFAC=MAX(0.0,(E0-EC)/EF)
	IF(EFAC.LT.85.0) Y0=EXP(-EFAC)
	DO I=1,NEN
	  DE=EUBOUND(I)-E0
	  E=E0+DE/2
	  EFAC=MAX(0.0,(E-EC)/EF)
	  IF(EFAC.LT.85.0)THEN
	    Y1=EXP(-EFAC)
	    E=EUBOUND(I)
	    EFAC=MAX(0.0,(E-EC)/EF)
	    Y2=EXP(-EFAC)
	    CALL MATH_INTEGRT(1.0,Y0,Y1,Y2,FLUX(I))
	  ELSE
	    FLUX(I)=0.0			! Large -ve exponent
	  ENDIF
	  E0=E
	  Y0=Y2
	ENDDO

*    Exit
 9000 IF ( STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'SPEC_EC', STATUS )
      END IF

      END
