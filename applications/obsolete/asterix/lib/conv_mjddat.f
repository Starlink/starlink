*+  CONV_MJDDAT - Converts MJD (D.P.) to calendar date and time (char string).
	SUBROUTINE CONV_MJDDAT(DMJD,DAT)
*    Description :
*    History :
*        Author	Clive Page	1985 Jan 17
*        Modified CGP 1985 Sept 2: put month name in 3-letter form.
*        May 10 1988 Asterix88 version (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
	DOUBLE PRECISION DMJD  !Modified Julian Date (IAU definition, of course)
*    Export :
	CHARACTER*(*) DAT      !Date and time as 20-char string in form
*                              !            "YYYY-MMM-DD hh:mm:ss"
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
	CHARACTER HMS*8, STRING*20, MONAME(12)*3
        INTEGER MJD
        INTEGER IYEAR,MONTH
        INTEGER IDATE
        REAL DAY
*    Local Data :
	DATA MONAME/'Jan','Feb','Mar','Apr','May','Jun',
     &   'Jul','Aug','Sep','Oct','Nov','Dec'/
*-
*
	MJD = INT(DMJD)
	DAY = DMJD - MJD
*
	CALL CONV_MJDYMD(MJD,IYEAR,MONTH,IDATE)
	CALL CONV_DAYHMS(DAY,HMS)
*
	WRITE(STRING,11)IYEAR,MONAME(MONTH),IDATE,HMS
11	FORMAT(I4,'-',A3,'-',I2.2,1X,A8)
*
	DAT = STRING
*
	END
