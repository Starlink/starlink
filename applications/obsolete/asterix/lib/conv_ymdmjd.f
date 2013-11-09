*+  CONV_YMDMJD - Converts calendar year+month+date to Modified Julian Date.
	SUBROUTINE CONV_YMDMJD(IYEAR,MONTH,IDAY,MJD)
*    Description :
*    History :
*      Author: C G Page, 1982 Jan 15;  Algorithm from
*                                               "Almanac for Computers 1982".
*      MAY 10 1988   Asterix88 version (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER IYEAR	!Year value, 4 digits, range 1901 to 2099.
      INTEGER MONTH	!Month value, range 1 - 12.
      INTEGER IDAY	!Date in month, range 1 - 31.
*                       !        (May also be 1-366 if MONTH=1)
*    Export :
      INTEGER MJD	!Modified Julian Date.
*    Local variables :
*     <declarations for local variables>
*-
*
	MJD = 367 * IYEAR - (7*(IYEAR + ((MONTH+9)/12))/4) +
     &   ((275 * MONTH)/9) + IDAY - 678987
*
	END
