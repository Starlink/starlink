*+ SPEC_BP - Computes flux from pseudo-bremsstrahlung spectrum
      SUBROUTINE SPEC_BP(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns flux in each energy bin delimited by bounds from a model
*     of form:
*			A*E**(-G)*exp(-E/T)	photons/(cm**2*s*keV)
*     where
*	A = PARAM(1)
*	G = PARAM(2)
*	T = PARAM(3)
*     G=1.4 corresponds roughly to a thermal bremsstrahlung model.
*     NOTE - This model is actually exactly the same as the cutoff power law
*     although starting parameter values are likely to differ.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Method :
*     Quadratic integral approximation used.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     15 Feb 85 : Original (TJP)
*      4 Apr 85 : Generally tidied up  (TJP)
*     20 Apr 88 : Lower and upper energy bounds passed in separate arrays (TJP)
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
	REAL G				! Energy index for 'Gaunt factor'
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
	G=PARAM(2)
	T=PARAM(3)

* T=0 case
	IF(T.EQ.0.0)THEN
	  DO I=1,NEN
	    FLUX(I)=0.0
	  ENDDO
	  GO TO 9000
	ENDIF

* Integrate over each energy channel
	E0=ELBOUND(1)
	Y0=E0**(-G)*EXP(-E0/T)
	DO I=1,NEN
	  DE=EUBOUND(I)-E0
	  E=E0+DE/2
	  Y1=E**(-G)*EXP(-E/T)
	  E=EUBOUND(I)
	  Y2=E**(-G)*EXP(-E/T)
	  CALL MATH_INTEGRT(DE,Y0,Y1,Y2,F)
	  FLUX(I)=PARAM(1)*F
	  E0=E
	  Y0=Y2
	ENDDO

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_BP',STATUS)
	END
