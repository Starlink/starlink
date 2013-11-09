*+  SPEC_BB_PLANCK - returns unnormalised Planck function
      REAL FUNCTION SPEC_BB_PLANCK(E,T)
*    Description :
*     Uncrashable calculation of unnormalised Planck function.
*    History :
*      3 Oct 86: Original  (BHVAD::TJP)
*     14 Dec 92: Protection aginst zero temp (TJP)
*    Type definitions :
      IMPLICIT NONE
*    Import :
	REAL E			! Photon energy
	REAL T			! Blackbody temperature (in energy units)
*    Global variables :
*    Local variables :
	REAL R			! Ratio E/T
*-

	IF(T.GT.0)THEN
	  R=E/T
	  IF(R.LE.0.0.OR.R.GT.80.0)THEN
	    SPEC_BB_PLANCK=0.0
	  ELSE
	    SPEC_BB_PLANCK=E**2/(EXP(R)-1)
	  ENDIF
	ELSE
	  SPEC_BB_PLANCK=0.0
	ENDIF

	END
