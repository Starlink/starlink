*+ SPEC_AC - Computes flux from cyclotron absorption formula
      SUBROUTINE SPEC_AC(NEN,ELBOUND,EUBOUND,PARAM,FLUX,STATUS)
*    Description :
*     Returns cyclotron absorption function:
*
*	    EXP(- D*E**2 / ( (E-EA)**2 + W**2 ) )
*
*       which approximates the optical depth of the cyclotron resonance
*       scattering in a uniform magnetic field
*           - Makishima et al 1990 Pub. Ast. Soc. Japan 42 295.
*
*
*      D = Thomson depth at E >> Ea = PARAM(1)
*      EA = non rel. cyclotron energy = 11.6(B/10**12 Gauss) keV = PARAM(2)
*      W = artificial broadening, keV = PARAM(3)
*
*    Method :
*     Quadratic integral approximation used for each bin.
*     NOTE: Contiguous energy bins are assumed (for efficiency savings).
*    Deficiencies :
*    Bugs :
*    Authors :
*     Sarah Unger (SOTON::SJU)
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     30 Nov 90 : Original
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
        REAL D                          ! Thomson depth
        REAL W                          ! Artificial broadening (HWHM)
        REAL EA                         ! Non rel. cycl. energy
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
        D    =PARAM(1)
        EA   =PARAM(2)
        W    =PARAM(3)

* Integrate over each energy channel
	 E0=ELBOUND(1)
	 Y0=EXP(-D*E0**2 / ( (E0-EA)**2 + W**2 ))
	 DO I=1,NEN
	   DE=EUBOUND(I)-E0
	   E=E0+DE/2
	   Y1=EXP(-D*E**2 / ( (E-EA)**2 + W**2 ))
	   E=EUBOUND(I)
	   Y2=EXP(-D*E**2 / ( (E-EA)**2 + W**2 ))
	   CALL MATH_INTEGRT(1.0,Y0,Y1,Y2,FLUX(I))
	   E0=E
	   Y0=Y2
	 ENDDO
*
* Exit
9000    IF(STATUS.NE.SAI__OK) CALL ERR_REP('EXERR','from SPEC_AC',
     :  STATUS)
	END
