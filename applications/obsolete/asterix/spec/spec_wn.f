*+ SPEC_WN - Computes flux from Wien spectrum
      SUBROUTINE SPEC_WN(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns flux in each energy channel delimited by bounds from a model
*     of form:
*			A*E**2*exp(-E/T)	photons/(cm**2*s*keV)
*     where
*	A = PARAM(1)
*	T = PARAM(2)
*     Note - this is almost identical to a black body spectrum for E>>T.
*    Method :
*     Quadratic integral approximation used.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*      4 Apr 85 : Original (TJP)
*     27 Apr 88 : Lower and upper energy bounds passed in separate arrays (TJP)
*     25 Feb 93 : Error handling corrected (DJA)
*      8 Mar 93 : EXPs made D.P. to cure UNIX floating overflow problems (DJA)
*
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

* T=0 case
	IF(T.EQ.0.0)THEN
	  DO I=1,NEN
	    FLUX(I)=0.0
	  ENDDO
	  GO TO 9000
	ENDIF

* Integrate over each energy channel
	E0=ELBOUND(1)
	Y0=E0**2*DEXP(DBLE(-E0/T))
	DO I=1,NEN
	  DE=EUBOUND(I)-E0
	  E=E0+DE/2
	  Y1=E**2*DEXP(DBLE(-E/T))
	  E=EUBOUND(I)
	  Y2=E**2*DEXP(DBLE(-E/T))
	  CALL MATH_INTEGRT(DE,Y0,Y1,Y2,F)
	  FLUX(I)=PARAM(1)*F
	  E0=E
	  Y0=Y2
	ENDDO

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_WN',STATUS)
	END
