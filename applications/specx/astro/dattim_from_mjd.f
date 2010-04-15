      SUBROUTINE DATTIM_FROM_MJD( MJD, IDATE, ITIME, STATUS)
*+
*  Name:
*     DATTIM_FROM_MJD

*  Purpose:
*     Convert Modified Julian Date to Specx date string formats

*  Invocation:
*     CALL DATTIM_FROM_MJD( MJD, IDATE, ITIME, STATUS)

*  Purpose:
*     Converts MJD (JD - 2400000.5) to the standard IDATE and
*     ITIME specx internal strings.

*  Arguments:
*     MJD = DOUBLE PRECISION (Given)
*        Stadndard modified Julian Date (JD - 2400000.5).
*     IDATE = CHARACTER (Returned)
*        Specx UT date string in DD-MON-YY format
*     ITIME = CHARACTER (Returned)
*        Specx UT time string in HH:MM:SS format
*     STATUS = INTEGER (Given & Returned)
*        Global Status. Returns non-zero on error

*  Author:
*     Tim Jenness (JAC, Hawaii)

*  History:
*     6 Jan 2000 (TIMJ):
*        Original Version

*.

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      DOUBLE PRECISION MJD

*  Arguments Returned:
      CHARACTER * (*) IDATE
      CHARACTER * (*) ITIME

*  Global Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER DD                ! Day in month
      DOUBLE PRECISION FD       ! Fraction of day
      INTEGER IHMSF(4)          ! hour, minute, second, frac
      CHARACTER * ( 3 ) MONTHS( 12 ) ! A calendar
      INTEGER MM                ! Month number
      CHARACTER *1 SIGN         ! Sign of time (+/-)
      INTEGER YY                ! year (includes century)

*  Local Data:
      DATA MONTHS / 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     :              'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /

*-

      IF ( STATUS .NE. 0) RETURN

*     Convert the MJD to year, month, day and fraction of day
      CALL SLA_DJCL( MJD, YY, MM, DD, FD, STATUS)

      IF ( STATUS .NE. 0) RETURN

*     Write the values into the string
      WRITE (IDATE, '(I2.2,''-'',A3,''-'',I2.2)')
     &                DD, MONTHS(MM), MOD(YY,100)

*     First convert the day fraction to Hours, minutes and seconds
      CALL SLA_DD2TF(3, FD, SIGN, IHMSF)

*     Write the values into the string
      WRITE (ITIME, '(I2.2,'':'',I2.2,'':'',I2.2)')
     &       IHMSF(1), IHMSF(2), IHMSF(3)


      END
