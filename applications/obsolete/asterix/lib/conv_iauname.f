*+  CONV_IAUNAME - Converts source RA, DEC (degs) to standard IAU form 'hhmm+ddd'
	SUBROUTINE CONV_IAUNAME(RA,DEC,NAME)
*    Description :
*     <description of what the subroutine does>
*    History :
*       Author	Clive Page	1982 Aug 3.
*       Oct 1988    Asterix88 version        (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Import :
      REAL RA               ! Right ascension, degrees (range 0 to 360)
      REAL DEC              ! Declination, degrees (range -90 to +90)
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
      CHARACTER NAME*8      ! Name of source in form 'hhmm+ddd' by
*                                           truncation of coordinates.
*    Local constants :
        INTEGER IRA
*-
	IRA = RA * 4.0
*
	WRITE(NAME,11)IRA/60, MOD(IRA,60), INT(DEC*10)
11	FORMAT(2I2.2,SP,I4.3)
*
	END
