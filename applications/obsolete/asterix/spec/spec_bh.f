*+ SPEC_BH - Computes flux from hydrogen bremss spectrum (Karzas & Latter)
      SUBROUTINE SPEC_BH(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns flux in each energy bin delimited by bounds from a model
*     of form:
*			A*g(E,T)*E**-1*exp(-E/T)	photons/(cm**2*s*keV)
*     where
*	A = PARAM(1)
*	T = PARAM(2)
*     and g(E,T) is the Gaunt factor for a fully ionized hydrogen plasma
*     derived by Karzas & Latter (1961) and tabulated by Kellog, Baldwin &
*     Koch (1975). A more accurate model is now available, but is more
*     complicated (Gould 1980) - see SPEC_BR.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Method :
*     Quadratic integral approximation used.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     24 Apr 85 : Original
*      2 Jun 87 : Small mod to FBG to avoid overflow at high E/kT (TJP)
*     20 Apr 88 : Lower and upper energy bounds passed in separate arrays (TJP)
*     11 Aug 88 : Logic tidied up (MPW/TJP)
*      2 Mar 89 : ASTERIX88 version, FBG renamed to SPEC_BH_FBG (TJP)
*     25 Feb 93 : Error handling corrected (DJA)
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
	REAL SPEC_BH_FBG
*    Local variables :
	INTEGER I
	REAL T				! Temperature in keV
	REAL E				! Current energy
	REAL E0				! Base energy for channel
	REAL DE				! Channel width
	REAL Y0,Y1,Y2			! Unnormalised function at bottom,
					! middle & top of channel
	REAL F				! Unnormalised integral
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
	T=PARAM(2)

	IF(T.NE.0.0) THEN

* Integrate over each energy channel
	  E0=ELBOUND(1)
	  E=E0
	  Y0=SPEC_BH_FBG(E,T)/E*EXP(-E/T)
	  DO I=1,NEN
	    DE=EUBOUND(I)-E0
	    E=E0+DE/2
	    Y1=SPEC_BH_FBG(E,T)/E*EXP(-E/T)
	    E=EUBOUND(I)
	    Y2=SPEC_BH_FBG(E,T)/E*EXP(-E/T)
	    CALL MATH_INTEGRT(DE,Y0,Y1,Y2,F)
	    FLUX(I)=PARAM(1)*F
	    E0=E
	    Y0=Y2
	  ENDDO

	ELSE

* T=0 case
	  DO I=1,NEN
	    FLUX(I)=0.0
	  ENDDO

	ENDIF

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_BH',STATUS)
	END
