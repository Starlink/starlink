*+ SPEC_PC - Computes flux from cut off power law spectrum
      SUBROUTINE SPEC_PC(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns flux in each energy channel delimited by bounds from a model
*     of form:
*		A*E**(-ALPHA)*EXP(-E/ECUT)    photons/(cm**2*s*keV)
*     where
*	A = PARAM(1)
*	ALPHA = PARAM(2)
*	ECUT=PARAM(3)
*    Method :
*     Quadratic integral approximation used for each bin.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*      3 Apr 85 : Original
*     27 Apr 88 : Lower and upper energy bounds passed in separate arrays (TJP)
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
	REAL ALPHA			! Power law index
	REAL ECUT			! Cutoff energy
	REAL E				! Current energy
	REAL E0				! Base energy for channel
	REAL DE				! Channel width
	REAL Y0,Y1,Y2			! Unnormalised function at
					! bottom, middle & top of channel
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
	ALPHA=PARAM(2)
	ECUT=PARAM(3)

* ECUT=0 case
	IF(ECUT.EQ.0.0)THEN
	  DO I=1,NEN
	    FLUX(I)=0.0
	  ENDDO
	  GO TO 9000
	ENDIF

* Integrate over each energy channel
	E0=ELBOUND(1)
	Y0=E0**(-ALPHA)*EXP(-E0/ECUT)
	DO I=1,NEN
	  DE=EUBOUND(I)-E0
	  E=E0+DE/2
	  Y1=E**(-ALPHA)*EXP(-E/ECUT)
	  E=EUBOUND(I)
	  Y2=E**(-ALPHA)*EXP(-E/ECUT)
	  CALL MATH_INTEGRT(DE,Y0,Y1,Y2,F)
	  FLUX(I)=PARAM(1)*F
	  E0=E
	  Y0=Y2
	ENDDO

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_PC',STATUS)
	END
