      INTEGER FUNCTION JULDA(NYR)
*+
*  Name:
*     JULDA

*  Purpose:
*     Calculate Julian day number from year 1900

*  Invocation:
*     JULIAN_DAY = JULDA ( NYR )

*  Description:
*     This function computes the Julian day number (modified) at 12hrs U.T.
*     on January 0 of the year nyr. JULDA is an integer because of this
*     definition. It does not use standard Modified Julian Date.
*     The reference date is actually 1900 January 0 0.5UT = Julian day
*     2415020.0

*  Arguments:
*     NYR = INTEGER (Given)
*       Year number. 2-digit years are treated as from 00-99 AD

*  Returned Value:
*     JULDA = INTEGER
*        Number of days from Julian day 2415020.0

*  Notes:
*     To convert to true Modified Julian Date (MJD) add 15019.5

*  Authors:
*     Rachael Padman (MRAO, Cambridge)
*     Tim Jenness (JAC, Hawaii)

*  History:
*     Pre-history (rp):
*        Original version. Uses integer arithmetic
*     30 Dec 1999 (timj):
*        Add descriptive header

*.

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER NYR

*  Local Variables:
      INTEGER IC                ! int(year / 100)
      INTEGER JULDAT            ! Julian date
      INTEGER NYRM1             ! Year minus 1

*-

      NYRM1=NYR-1
      IC=NYRM1/100
      JULDAT=1721425+365*NYRM1+NYRM1/4-IC+IC/4
      JULDA=JULDAT-2415020

      RETURN
      END
