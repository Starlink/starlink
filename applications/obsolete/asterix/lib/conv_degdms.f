*+  CONV_DEGDMS - Converts decimal degrees to degrees, minutes, seconds of arc.
	SUBROUTINE CONV_DEGDMS(DEGS, DMS)
*    Description :
*    History :
*      Author	Clive Page	1987 MARCH 3
*      Oct 1988    Asterix88 version         (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      REAL DEGS		      ! angle in degrees.
*    Export :
      CHARACTER*(*) DMS	      ! character string (>=9 chars normally).
*    Local constants :
*    Local variables :
	CHARACTER SIGN
        INTEGER NSECS,ISECS
        INTEGER MINS,IMINS
        INTEGER IDEGS
        INTEGER K
*
*-
	IF(DEGS .GE. 0.0) THEN
		SIGN = '+'
	ELSE
		SIGN = '-'
	END IF
*
	NSECS = NINT(ABS(DEGS) * 3600.0)
	ISECS = MOD(NSECS, 60)
	MINS  = NSECS / 60
	IMINS = MOD(MINS, 60)
	IDEGS = MINS / 60
*
	WRITE(DMS,'(I3,A1,I2.2,A1,I2.2)',IOSTAT=K)
     :                                     IDEGS,'d',IMINS,'m',ISECS
        DO WHILE (DMS(1:1).EQ.' ')
          DMS=DMS(2:)
        ENDDO
        DMS=SIGN//DMS
*
	END
