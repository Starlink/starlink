*+  CONV_DAYHMS - Converts fractions of a day to hours, mins, secs as char string
	SUBROUTINE CONV_DAYHMS(DAY,HMS)
*    Description :
*    History :
*        Author	Clive Page (original)	1985 Jan 17 (LTVAD::CGP)
*        10 May 1988  Asterix88 version R.D.Saxton (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
	REAL DAY            !Fraction of a day, should be >=0.0 and <86400.0
*    Export :
	CHARACTER*(*) HMS   !Time of day as 8-char string in form "HH:MM:SS".
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
	CHARACTER STRING*8
        INTEGER ISECS,IMINS,IHOUR
*-
*Start of code
*
	ISECS = INT(MOD(DAY,1.0)*86400.0)
	IHOUR = ISECS / 3600
	ISECS = ISECS - 3600 * IHOUR
	IMINS = ISECS / 60
	ISECS = ISECS - 60 * IMINS
*
	WRITE(STRING,11)IHOUR,IMINS,ISECS
11	FORMAT(I2.2,2(':',I2.2))
*
	HMS = STRING
*
	END
