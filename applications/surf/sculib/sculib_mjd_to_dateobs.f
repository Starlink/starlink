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

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2000 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     06-Jan-2000 (TIMJ):
*        First version for SPECX
*     22-Jun-2000 (TIMJ):
*        Converted to SCULIB with inherited status
*     15-JUN-2008 (TIMJ):
*        Drop Z from suffix (not part of standard).
*        Always write modern format even in old data.

*  Notes:
*     Always write out the DATE-OBS string in modern format. Originally
*     this routine would use "dd/mm/yy" for dates earlier than 1999 but this
*     makes no sense. Now uses ISO8601 as modified by FITS (no Z).

*-

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

*  Local Variables:
      INTEGER DD                ! Day in month
      DOUBLE PRECISION FD       ! Fraction of day
      INTEGER IHMSF(4)          ! hour, minute, second, frac
      INTEGER MM                ! Month number
      CHARACTER *1 SIGN         ! Sign of time (+/-)
      INTEGER SLA_STATUS        ! SLALIB status
      INTEGER YY                ! year (includes century)
*.

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

*     First convert the day fraction to Hours, minutes and seconds
         CALL SLA_DD2TF(3, FD, SIGN, IHMSF)

*     Insert the values into the string
         WRITE (DATE_OBS, '(I4.4,''-'',I2.2,''-'',I2.2,''T'',
     :        I2.2,'':'',I2.2,'':'',I2.2,''.'',I3.3)')
     :        YY, MM, DD, IHMSF(1), IHMSF(2), IHMSF(3), IHMSF(4)


      END
