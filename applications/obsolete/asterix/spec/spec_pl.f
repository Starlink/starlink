*+ SPEC_PL - Computes flux from power law spectrum
      SUBROUTINE SPEC_PL(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns flux in each energy channel delimited by bounds from a model
*     of form:
*			A*E**(-ALPHA)    photons/(cm**2*s*keV)
*     where
*	A = PARAM(1)
*	ALPHA = PARAM(2)
*    Method :
*     Exact integral is calculated.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*      4 Feb 85 : Original
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
	REAL PARAM(2)			! Model parameters
*    Import-Export :
*    Export :
	REAL FLUX(NEN)			! Photon flux (phot/cm**2/s)
*    Status :
	INTEGER STATUS
*    Local variables :
	INTEGER I
	REAL ALPHA				! Power law index
	REAL POWER				! 1-ALPHA
	REAL X1,X2,C
*-

* Status check
	IF(STATUS.NE.SAI__OK) RETURN

* Check for spot value
	IF(ELBOUND(1).EQ.EUBOUND(1))THEN
	  STATUS=SAI__ERROR
	  CALL ERR_REP('SPOT','Spot values not supported by SPEC_ routines',
     :    STATUS)
	  GOTO 9000
	ENDIF

* Model parameters
	ALPHA=PARAM(2)
	IF(ABS(ALPHA-1).LT.0.0001)THEN

* Special case - logarithmic integral
	  X1=ALOG(ELBOUND(1))
	  DO I=1,NEN
	    X2=ALOG(EUBOUND(I))
	    FLUX(I)=PARAM(1)*(X2-X1)
	    X1=X2
	  ENDDO
	ELSE

* General power law
	  POWER=1-ALPHA
	  C=PARAM(1)/POWER
	  X1=ELBOUND(1)**POWER
	  DO I=1,NEN
	    X2=EUBOUND(I)**POWER
	    FLUX(I)=C*(X2-X1)
	    X1=X2
	  ENDDO
	ENDIF

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_PL',STATUS)
	END
