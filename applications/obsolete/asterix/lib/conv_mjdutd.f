*+  CONV_MJDUTD - Converts MJD to year and day-in-year.
	SUBROUTINE CONV_MJDUTD(MJD,IYEAR,IDAY)
*    Description :
*    Restriction: only valid for MJDs in this century.
*    History :
*       Author	Clive Page	1984 Aug 28.
*       May 10 1988   Asterix88 version   (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
	INTEGER MJD       !Modified Julian Date.
*    Export :
	INTEGER IYEAR     !Year number, range 1901 - 1999.
        INTEGER IDAY      !Day number in year, range 1 - 366.
*    Local variables :
        INTEGER NYEAR
        INTEGER MJDSYR
*-
*
	NYEAR  = (MJD - 15019) / 365.2499
	MJDSYR = (NYEAR * 365.25) + 15018.9
	IYEAR  = NYEAR + 1900
	IDAY   = MJD - MJDSYR
*
	END
