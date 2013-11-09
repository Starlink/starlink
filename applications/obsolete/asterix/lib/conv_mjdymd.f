*+  CONV_MJDYMD - Converts MJD to calendar year, month, date.
	SUBROUTINE CONV_MJDYMD(MJD,IYEAR,MONTH,IDATE)
*    Description :
*     <description of what the subroutine does>
*    History :
*     Author	Clive Page	1984 Aug 28.
*     Jun 24 1988   Asterix88 version (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      INTEGER MJD            ! Modified Julian Date.
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      INTEGER IYEAR          ! Year number, range 1901 - 1999.
      INTEGER MONTH          ! Month in year, range 1 - 12.
      INTEGER IDATE          ! Date in month, range 1 - 31.
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER IDAY
*-
      CALL CONV_MJDUTD(MJD,IYEAR,IDAY)
      CALL CONV_UTDYMD(IYEAR,IDAY,MONTH,IDATE)
*
      END
