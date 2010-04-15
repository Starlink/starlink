      DOUBLE PRECISION FUNCTION SPECXJD_TO_MJD(SPXJD)
*+
*  Name:
*     SPECXJD_TO_MJD

*  Purpose:
*     Convert specx modified Julian date to standard MJD

*  Invocation:
*     MJD = SPECXJD_TO_MJD( SPXJD )

*  Description:
*     Specx does not use the standard epoch for its calculation
*     of modified Julian date. It uses midday on Jan 00 1900
*     (Julian date of 2415020.0). This routine simply accounts
*     for this and returns the standard definition of Modified
*     Julian Date (days from Julian date of 2400000.5)

*  Arguments:
*     SPXJD = DOUBLE PRECISION (Given)
*        Specx Modified Julian Day referenced to JD2415020.0.

*  Returned Value:
*     SPECXJD_TO_MJD = DOUBLE PRECISION
*        Standard definition of MJD referenced to JD2400000.5

*  Authors:
*     Tim Jenness (JAC, Hawaii)

*  History:
*     06-Jan-2000 (TIMJ):
*        First version

*  Notes:
*     The specx reference date is set in ASTRO/JULDA.F in addition
*     to this routine. I believe it is also assumed in ASTRO/LOSTIM2.F

*.

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      DOUBLE PRECISION SPXJD

*  Local Constants:
      DOUBLE PRECISION SPXREFJD ! Specx reference Julian Date
      PARAMETER ( SPXREFJD = 2415020.0 )
      DOUBLE PRECISION REFJD    ! Standard MJD reference JD
      PARAMETER ( REFJD = 2400000.5 )

*-

      SPECXJD_TO_MJD = SPXJD + SPXREFJD - REFJD

      END
