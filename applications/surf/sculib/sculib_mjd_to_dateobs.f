      SUBROUTINE SCULIB_MJD_TO_DATEOBS (MJD, DATE_OBS, STATUS)
*+
*  Name:
*     SCULIB_MJD_TO_DATEOBS

*  Purpose:
*     Convert a modified julian date to the correct FITS DATE-OBS format

*  Invocation:
*     CALL SCULIB_MJD_TO_DATEOBS ( MJD, DATE_OBS, STATUS )

*  Description:
*     Given a modified Julian date  generate
*     a correctly formatted FITS DATE-OBS string.

*  Arguments:
*     MJD = DOUBLE PRECISION (Given)
*        Modified Julian date (Defined as JD - 2400000.5)
*     DATE_OBS = CHARACTER * (*) (Returned)
*        FITS DATE-OBS string. Should have at least 24
*        characters for YYYY-MM-DDThh:mm:ss.sssZ format
*     STATUS = INTEGER (Given & Returned)
*        Global Status

*  Authors:
*     Tim Jenness (JAC, Hawaii)

*  History:
*     06-Jan-2000 (TIMJ):
*        First version for SPECX
*     22-Jun-2000 (TIMJ):
*        Converted to SCULIB with inherited status

*  Notes:
*     The format used for the DATE-OBS keyword depends on the value of the
*     keyword. For DATE-OBS < 1999.0, use the old "dd/mm/yy" format.
*     Otherwise, use the new "ccyy-mm-ddThh:mm:ss[.ssss]Z" format.

*.

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION MJD

*  Arguments Returned:
      CHARACTER * (*) DATE_OBS

*  Global status:
      INTEGER STATUS

*  Local constants:
      DOUBLE PRECISION MJD99    ! MJD of 1 Jan 1999
      PARAMETER ( MJD99 = 51179.0D0 )

*  Local Variables:
      INTEGER DD                ! Day in month
      DOUBLE PRECISION FD       ! Fraction of day
      INTEGER IHMSF(4)          ! hour, minute, second, frac
      INTEGER MM                ! Month number
      CHARACTER *1 SIGN         ! Sign of time (+/-)
      INTEGER SLA_STATUS        ! SLALIB status
      INTEGER YY                ! year (includes century)
*-

      IF (STATUS .NE. SAI__OK) RETURN

*     Convert the MJD to year, month, day and fraction of day
      CALL SLA_DJCL( MJD, YY, MM, DD, FD, SLA_STATUS)

*     Check status and return 00/00/00 if necessary
      IF (SLA_STATUS .NE. 0) THEN

         STATUS = SAI__ERROR
         CALL MSG_SETD('MJD',MJD)
         CALL ERR_REP(' ','WRITE_MAP_INFO: Error converting'//
     :        ' MJD (^MJD) to YYYY-MM-DD format using SLA_DJCL',
     :        STATUS)

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
         WRITE (DATE_OBS, '(I4.4,''-'',I2.2,''-'',I2.2,''T'',
     :        I2.2,'':'',I2.2,'':'',I2.2,''.'',I3.3,''Z'')')
     :        YY, MM, DD, IHMSF(1), IHMSF(2), IHMSF(3), IHMSF(4)


      END IF

      END
