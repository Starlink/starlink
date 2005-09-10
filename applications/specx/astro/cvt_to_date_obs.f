      SUBROUTINE CVT_TO_DATE_OBS(MJD, DATE_OBS)
*+
*  Name:
*     CVT_TO_DATE_OBS

*  Purpose:
*     Convert a modified julian date to the correct FITS DATE-OBS format

*  Invocation:
*     CALL CVT_TO_DATE_OBS ( MJD, DATE_OBS )

*  Description:
*     Given a modified Julian date (as used by SLALIB -- not
*     using the specx epoch: use SPECXJD_TO_MJD to convert) generate
*     a correctly formatted FITS DATE-OBS string.

*  Arguments:
*     MJD = DOUBLE PRECISION (Given)
*        Modified Julian date (Defined as JD - 2400000.5)
*     DATE_OBS = CHARACTER * (*) (Returned)
*        FITS DATE-OBS string. Should have at least 23
*        characters for YYYY-MM-DDThh:mm:ss.sss format

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Alan Chipperfield (Starlink, RAL)

*  History:
*     06-Jan-2000 (TIMJ):
*        First version
*     20-Sep-2000 (AJC):
*        Don't split strings across lines
*     09-Sep-2005 (TIMJ):
*        Trailing Z is not part of the FITS standard since FITS enforces
*        the use of UTC.

*  Notes:
*     The format used for the DATE-OBS keyword depends on the value of the
*     keyword. For DATE-OBS < 1999.0, use the old "dd/mm/yy" format.
*     Otherwise, use the new "ccyy-mm-ddThh:mm:ss[.ssss]" format.

*.

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      DOUBLE PRECISION MJD

*  Arguments Returned:
      CHARACTER * (*) DATE_OBS

*  Local Variables:
      INTEGER DD                ! Day in month
      DOUBLE PRECISION FD       ! Fraction of day
      INTEGER IHMSF(4)          ! hour, minute, second, frac
      DOUBLE PRECISION MJD99    ! MJD of the transition date
      INTEGER MM                ! Month number
      CHARACTER *1 SIGN         ! Sign of time (+/-)
      INTEGER STATUS            ! SLALIB status
      INTEGER YY                ! year (includes century)
*-

*     Calculate the transition date as 1999-01-01
      CALL SLA_CALDJ( 99, 1, 1, MJD99, STATUS)

*     Convert the MJD to year, month, day and fraction of day
      CALL SLA_DJCL( MJD, YY, MM, DD, FD, STATUS)

*     Check status and return 00/00/00 if necessary
      IF (STATUS .NE. 0) THEN

         PRINT *,' -- CVT_TO_DATE_OBS --'
         PRINT *,' error converting MJD to DATE-OBS'
         PRINT *,' using 00/00/00'

         DATE_OBS = '00/00/00'

         RETURN
      END IF

*     Is the date above or below the transition
      IF (MJD .LT. MJD99) THEN

*     Convert year to 2-digit. Note that we already know that
*     we are less than 2000 because of the above test
         YY = MOD(YY, 100)

         WRITE (DATE_OBS, '(I2.2,''/'',I2.2,''/'',I2.2)') DD, MM, YY

      ELSE
*     We are above the transition date so we must use the new FITS
*     DATE-OBS format

*     First convert the day fraction to Hours, minutes and seconds
         CALL SLA_DD2TF(3, FD, SIGN, IHMSF)

*     Insert the values into the string
         WRITE (DATE_OBS, '(I4.4,''-'',I2.2,''-'',I2.2,''T'','//
     :        'I2.2,'':'',I2.2,'':'',I2.2,''.'',I3.3)')
     :        YY, MM, DD, IHMSF(1), IHMSF(2), IHMSF(3), IHMSF(4)


      END IF

      END
