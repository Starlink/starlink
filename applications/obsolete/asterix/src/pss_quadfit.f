*+  PSS_QUADFIT - Quadratic fitting at minimum
      SUBROUTINE PSS_QUADFIT(XMIN, YMIN, X, Y, YFIT, XLO, XHI, STATUS )
*    Description :
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      REAL                  XMIN, YMIN             ! Position of minimum
      REAL                  X, Y                   ! Other position on quadratic
      REAL                  YFIT                   ! Requested Y value
*
*    Export :
*
      REAL                  XLO, XHI               ! The 2 intersection values
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                  DELX
*-

      IF ( STATUS .EQ. SAI__OK ) THEN
	IF(XMIN.NE.X)THEN
          DELX = ABS(X-XMIN)*SQRT((YFIT-YMIN)/(Y-YMIN))
          XLO = XMIN - DELX
          XHI = XMIN + DELX
	ELSE
          XLO = 0.
          XHI = 0.
          STATUS = 1
	END IF

      END IF

      END
