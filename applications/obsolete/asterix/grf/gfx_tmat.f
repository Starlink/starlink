*+ GFX_TMAT - construct transformation matrix
	SUBROUTINE GFX_TMAT(UNITS,RA,DEC,ROLL,STATUS)
*
*    Description:
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Import :
      CHARACTER*(*) UNITS
      DOUBLE PRECISION RA,DEC,ROLL
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_SKY_CMN'
*    Local constants :
	DOUBLE PRECISION PI, TWOPI, DTOR
	PARAMETER (PI = 3.14159265358979D0, TWOPI=2.0D0*PI,
     :             DTOR = PI/180.0D0)
*    Local variables :
        DOUBLE PRECISION ETOC(3,3)
        DOUBLE PRECISION GTOC(3,3)
	DOUBLE PRECISION VPOL(3), VEQU(3),  DECPOL, CDUM, RAPOL
*    Local data :
	DATA ETOC /
     &   1.00000000000000D0, 0.00000000000000D0, 0.00000000000000D0,
     &   0.00000000000000D0, 9.17482062069182D-1, -3.97777155931914D-1,
     &   0.00000000000000D0, +3.97777155931914D-01, 9.17482062069182D-1/
	DATA GTOC /
     &   -0.054875539726d0, +0.494109453312d0, -0.867666135858d0,
     &   -0.873437108010d0, -0.444829589425d0, -0.198076386122d0,
     &   -0.483834985808d0, +0.746982251810d0, +0.455983795705d0/
*
      IF (STATUS .EQ. SAI__OK) THEN



*  get factor for conversion of units to radian
        CALL CONV_UNIT2R(UNITS,G_XYTORAD,STATUS)


        IF (STATUS.EQ.SAI__OK) THEN
* Convert angles into radians
	  RA  = RA  * DTOR
	  DEC = DEC * DTOR
	  ROLL = ROLL * DTOR

* Force ROLL to range -PI to PI
          ROLL = MOD(ROLL+TWOPI+PI,TWOPI) - PI

* Find Celestial position of local north pole (Z-axis)
	  DECPOL = ASIN(COS(ROLL) * COS(DEC))
	  CDUM   = ACOS(MIN(MAX(-1.0D0,-TAN(DECPOL) * TAN(DEC)),1.0D0))
	  IF(ROLL .GT. 0.0D0) THEN
	    RAPOL = RA + CDUM
	  ELSE
	    RAPOL = RA - CDUM
	  ENDIF
*
* Convert pole and origin to cartesian 3-vectors
	  CALL CONV_DONA2V(RAPOL, DECPOL, VPOL)
	  CALL CONV_DONA2V(RA,   DEC,    VEQU)
*
* Generate transform matrix celestial to instrument in array index 1.
	  CALL CONV_DONGEN(VPOL, VEQU, G_TMAT)
* Multiply by Cel==>Ecl to generate Ecl==>Instr in array index 2
	  CALL MATH_MATMULT(ETOC, G_TMAT(1,1,1), G_TMAT(1,1,2))
* Multiply by Cel==>Gal to generate Gal==>Instr in array index 3
	  CALL MATH_MATMULT(GTOC, G_TMAT(1,1,1), G_TMAT(1,1,3))


	ENDIF


        IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_TMAT', STATUS)
        ENDIF

      ENDIF

      END
