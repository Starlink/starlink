*+  MATH_INTEGRT - 3 point integral based on quadratic model
      SUBROUTINE MATH_INTEGRT(DX,Y0,Y1,Y2,YINT)
*
*    Description :
*
*     Given the Y values at the centre and boundaries of the interval
*     X0 to X0+DX, an estimate of the integral under Y(X) over this
*     interval is returned in YINT based on fitting a quadratic through
*     the three points given.
*
*    Method :
*
*     If A*X**2+B*X+C is the fitted quadratic, then the integral is
*     2*D*(A*D**2/3+Y1) ,where D=DX/2, which is independent of X0.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     29 Jun 87: Original (from INTEGRT) (TJP)
*    Type definitions :
	IMPLICIT NONE
*    Global constants :
*    Import :
	REAL DX		! Width of integration interval in X
	REAL Y0		! Y(X0)
	REAL Y1		! Y(X0+DX/2)
	REAL Y2		! Y(X0+DX)
*    Import-Export :
*    Export :
	REAL YINT	! Estimate of integral under Y(X) from
			! X0 to X0+2*DX
*    Status :
*    Local constants :
*    Local variables :
	REAL D		! DX/2
	REAL A		! Coefft of X**2
*-

* Check DX
	IF(DX.EQ.0)THEN
	  YINT=0.0
	  RETURN
	ENDIF

* Evaluate coefficients of X**2
	D=DX/2
	A=(Y0+Y2-2*Y1)/(2*D**2)

* Evaluate integral
	YINT=DX*(A*D**2/3+Y1)

	END



*+  MATH_INTEGRTD - 3 point integral based on quadratic model, DP version
      SUBROUTINE MATH_INTEGRTD(DX,Y0,Y1,Y2,YINT)
*    Description :
*     Given the Y values at the centre and boundaries of the interval
*     X0 to X0+DX, an estimate of the integral under Y(X) over this
*     interval is returned in YINT based on fitting a quadratic through
*     the three points given.
*    Method :
*     If A*X**2+B*X+C is the fitted quadratic, then the integral is
*     2*D*(A*D**2/3+Y1) ,where D=DX/2, which is independent of X0.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman  (BHVAD::TJP)
*    History :
*     29 Jun 87: Original (from INTEGRT) (TJP)
*    Type definitions :
	IMPLICIT NONE
*    Global constants :
*    Import :
      DOUBLE PRECISION DX		! Width of integration interval in X
      DOUBLE PRECISION Y0		! Y(X0)
      DOUBLE PRECISION Y1		! Y(X0+DX/2)
      DOUBLE PRECISION Y2		! Y(X0+DX)
*    Import-Export :
*    Export :
      DOUBLE PRECISION YINT		! Estimate of integral under Y(X) from
					! X0 to X0+2*DX
*    Status :
*    Local constants :
*    Local variables :
      DOUBLE PRECISION D		! DX/2
      DOUBLE PRECISION A		! Coefft of X**2
*-

* Check DX
	IF(DX.EQ.0)THEN
	  YINT=0.0D0
	  RETURN
	ENDIF

* Evaluate coefficients of X**2
	D=DX/2
	A=(Y0+Y2-2*Y1)/(2*D**2)

* Evaluate integral
	YINT=DX*(A*D**2/3+Y1)

	END
