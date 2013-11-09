*+ SPEC_BB - Computes flux from black body spectrum with luminosity norm
      SUBROUTINE SPEC_BB(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns flux in each energy bin delimited by bounds from a model
*     of form:
*		A*0.80525*E**2/(T**4*(exp(E/T)-1))	photons/(cm**2*s*keV)
*     where
*	A = PARAM(1) is the bolometric luminosity/distance^2 in units of
*                    (1E38 erg/s)/(10 kpc)**2
*	T = PARAM(2) is the temperature in keV
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Method :
*     Quadratic integral approximation used.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     18 Oct 85: Original
*     20 Apr 88: Lower and upper energy bounds passed in separate arrays (TJP)
*     14 Dec 92: Norm changed to luminosity units (TJP)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
	INTEGER NEN			! No of energy channels
	REAL ELBOUND(NEN)		! Lower bin bounds (keV)
	REAL EUBOUND(NEN)		! Upper bin bounds (keV)
	REAL PARAM(2)			! Model parameters
*    Import-Export :
*    Export :
	REAL FLUX(NEN)			! Photon flux (phot/cm**2/s)
*    Status :
	INTEGER STATUS
*    Function declarations :
	REAL SPEC_BB_PLANCK
*    Local variables :
	INTEGER I
	REAL T				! Temperature in keV
	REAL E				! Current energy
	REAL E0				! Base energy for channel
	REAL DE				! Channel width
	REAL Y0,Y1,Y2			! Unnormalised function at bottom,
					! middle & top of channel
	REAL F				! Unnormalised integral
	REAL NORM			! Normalisation factor
*-----------------------------------------------------------------------------

* Status check
	IF(STATUS.NE.SAI__OK) GO TO 9000

* Check for spot value
	IF(ELBOUND(1).EQ.EUBOUND(1))THEN
	  CALL ERR_REP('SPOT','Spot values not supported by SPEC_ routines',
     :    STATUS)
	  STATUS=SAI__ERROR
	  GO TO 9000
	ENDIF

* Parameters
	T=PARAM(2)
	IF(T.GT.0)THEN
	  NORM=PARAM(1)*0.80525/T**4
	ENDIF

* T<=0 case
	IF(T.LE.0.0)THEN
	  DO I=1,NEN
	    FLUX(I)=0.0
	  ENDDO
	  GO TO 9000
	ENDIF

* Integrate over each energy channel
	E0=ELBOUND(1)
	E=E0
	Y0=SPEC_BB_PLANCK(E,T)
	DO I=1,NEN
	  DE=EUBOUND(I)-E0
	  E=E0+DE/2
	  Y1=SPEC_BB_PLANCK(E,T)
	  E=EUBOUND(I)
	  Y2=SPEC_BB_PLANCK(E,T)
	  CALL MATH_INTEGRT(DE,Y0,Y1,Y2,F)
	  FLUX(I)=NORM*F
	  E0=E
	  Y0=Y2
	ENDDO

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_BB',STATUS)
	END
