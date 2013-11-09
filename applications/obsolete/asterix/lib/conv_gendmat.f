*+  CONV_GENDMAT - Generates DP transformation matrices given RA, DEC, rollangle.
	SUBROUTINE CONV_GENDMAT(DRA, DEC, DROLL, DCTOS)
*    Description :
*      Takes an RA,DEC and position-angle and produces an attitude matrix to
*     transform spacecraft co-ords to celestial.
*    History :
*         Author	Clive Page	1988 Oct 4
*         Modified for Asterix88     LTVAD::RDS     7 Nov 1988
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      DOUBLE PRECISION DRA		!	Right Ascension (radians).
      DOUBLE PRECISION DEC		!	Declination (radians).
      DOUBLE PRECISION DROLL		!	Position angle of y-axis
					!	anticlockwise N thro E (rads)
*    Import-Export :
*    Export :
      DOUBLE PRECISION DCTOS(3,3)	!	Matrices frame ==> spacecraft
*    Local constants :
      DOUBLE PRECISION PI, TWOPI
      PARAMETER (PI = 3.141592653589793D0, TWOPI = 2.0D0 * PI)
*    Local variables :
      DOUBLE PRECISION VPOL(3), VEQU(3), ROLL, DECPOL, CDUM, RAPOL
*
* Force ROLL to range -PI to PI
	ROLL = MOD(DROLL+TWOPI+PI,TWOPI) - PI
*
* Find Celestial position of local north pole (Z-axis)
	DECPOL = ASIN(COS(ROLL) * COS(DEC))
	CDUM   = ACOS(MIN(MAX(-1.0D0,-TAN(DECPOL) * TAN(DEC)),1.0D0))
	IF(ROLL .GT. 0.0D0) THEN
		RAPOL = DRA + CDUM
	ELSE
		RAPOL = DRA - CDUM
	END IF
*
* Convert pole and origin to cartesian 3-vectors
	CALL CONV_DONA2V(RAPOL, DECPOL, VPOL)
	CALL CONV_DONA2V(DRA,   DEC,    VEQU)
*
* Generate transform matrix celestial to space-craft in array index 1.
	CALL CONV_DONGEN(VPOL, VEQU, DCTOS)
*
	END
