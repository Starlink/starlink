*+  CONV_UTDYMD - Converts year and day-in-year to month and date.
	SUBROUTINE CONV_UTDYMD(IYEAR,IDAY,MONTH,IDATE)
*    Description :
*    History :
*       Author	Clive Page	1984 Aug 28.
*       May 10 1988   Asterix88 version (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
	INTEGER IYEAR       !Year number, 4 digits.
        INTEGER IDAY	    !Day in year, range 1 - 366.
*     <declarations and descriptions for imported arguments>
*    Import-Export :
*    Export :
        INTEGER MONTH	    !Month number, range 1 - 12.
        INTEGER IDATE	    !Date in month, range 1 - 31.
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
        INTEGER N,M
	INTEGER LENGTH(12)
	SAVE LENGTH
*    Initialisations :
	DATA LENGTH /31,28,31, 30,31,30, 31,31,30, 31,30,31/
*-
*
	IF(MOD(IYEAR,4) .EQ. 0) THEN
		LENGTH(2) = 29
	ELSE
		LENGTH(2) = 28
	END IF
	N = 0
	DO M = 1,12
		N = N + LENGTH(M)
		IF(N .GE. IDAY) GOTO 20
	END DO
20	CONTINUE
	MONTH = M
	IDATE = IDAY - N + LENGTH(M)
	END
