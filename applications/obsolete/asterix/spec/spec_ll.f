*+ SPEC_LL - Computes flux from a Lorentzian spectral line
      SUBROUTINE SPEC_LL(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns flux in each energy channel delimited by bounds from a Lorentzian
*     spectral line model:
*		(A*W/pi)/[(E-EL)**2+W**2]  photons/(cm**2*s*keV)
*     where
*	A = PARAM(1)	leading const
*	EL = PARAM(2)	line centre energy
*	W = PARAM(3)	line half width at half max (HWHM)
*    Method :
*     Integral under a Lorentzian line is evaluated analytically.
*     If W=0 then a delta function line is adopted.
*     The leading constant A is the integrated photons/(cm**2*s) in the line.
*     Note that if EL is not >>W then some of the line flux will be lost to
*     negative energies.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*      1 Jun 87 : Original (TJ)
*     27 Apr 88 : Lower and upper energy bounds passed in separate arrays (TJP)
*     25 Feb 93 : Error handling corrected (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
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
*    Local constants :
*    Local variables :
	INTEGER I
	REAL EL				! Line energy
	REAL W				! Line width (sigma)
	REAL CONST			! Const coefficient
	REAL PREV			! Integral to previous bound
	REAL CURR			! Integral to current bound
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
	EL=PARAM(2)
	W=PARAM(3)

* Zero FLUX array
	DO I=1,NEN
	  FLUX(I)=0.0
	ENDDO

* Delta function line
	IF(W.LE.0.0)THEN
	  IF(ELBOUND(1).GT.EL) GO TO 9000
	  DO I=1,NEN
	    IF(EUBOUND(I).GT.EL)THEN		! Note that a delta fn. line
	      FLUX(I)=PARAM(1)			! falling on a boundary is put
	      GO TO 9000				! into the lower channel
	    ENDIF
	  ENDDO
	ENDIF

* Set up
	CONST=PARAM(1)/MATH__PI
	PREV=ATAN((ELBOUND(1)-EL)/W)

* Integrate over each energy channel
	DO I=1,NEN
	  CURR=ATAN((EUBOUND(I)-EL)/W)
	  FLUX(I)=CONST*(CURR-PREV)
	  PREV=CURR
	ENDDO

* Exit
 9000	IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_LL',STATUS)
	END
