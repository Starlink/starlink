      SUBROUTINE PARSE_DATE_OBS( DATE_OBS, READUT, IDATE, ITIME, STATUS)
*+
*  Name:
*     PARSE_DATE_OBS

*  Purpose:
*     Convert a FITS DATE-OBS keyword to Specx DATE and TIME strings

*  Invocation:
*     CALL PARSE_DATE_OBS( DATE_OBS, READUT, IDATE, ITIME, STATUS)

*  Description:
*     Parses a FITS DATE-OBS keyword and converts it to specx DATE
*     format. Optionally, for dates with the new Y2K friendly format
*     with UT times attached, the specx TIME is also extracted. A Logical
*     is set to indicate that the time has been read.

*  Arguments:
*     DATE_OBS = CHARACTER (Given)
*        Input FITS DATE-OBS string.
*     READUT = LOGICAL (Returned)
*        TRUE is a UT date was extracted from the string. False otherwise.
*     IDATE = CHARACTER (Returned)
*        UT date in specx DD-MON-YY format. String should be 9 characters.
*     ITIME = CHARACTER (Returned)
*        UT time in specx hh:mm:ss format. String should be 8 characters.
*     STATUS = INTEGER (Given & Returned)
*        Global status

*  Author:
*     Tim Jenness (JAC, Hawaii)
*     Alan Chipperfield (Starlink)

*  History:
*     6 Jan 2000 (TIMJ)
*        Original version
*    21 Sep 2000 (AJC)
*        Unused IFAIL
*    10 Jun 2003 (TIMJ)
*        Linux does not like '(I4,''-''...)' embedded format style
*        so pull out as explicit format statements
*        Added CCYY-MM-DDTHH:MM format for completeness.
*    25 Oct 2012 (TIMJ)
*        Formats for READ are not allowed to include constant strings.
*        Both g95 and gfortran get upset so replace with 1X.

*  Notes:
*     Guesses at the format of the DATE-OBS string from its length.
*        DD/MM/YY                    8
*        CCYY-MM-DD                 10
*        CCYY-MM-DDTHH:MM           16
*        CCYY-MM-DDTHH:MM:SS        19
*        CCYY-MM-DDTHH:MM:SSZ       20
*        CCYY-MM-DDTHH:MM:SS.SSS    23
*        CCYY-MM-DDTHH:MM:SS.SSSZ   24
*     But note that the trailing Z is irrelevant for the parsing
*     of the DATE-OBS string by the READ command and that SPECX
*     dates do not care about fractional seconds.
*

*.

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      CHARACTER * (*) DATE_OBS

*  Arguments Returned:
      LOGICAL READUT
      CHARACTER * (*) ITIME
      CHARACTER * (*) IDATE

*  Global Status
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      INTEGER DD                ! Day number
      INTEGER FS                ! Fraction of second
      INTEGER HH                ! Number of hours
      INTEGER ILEN              ! Length of DATE-OBS
      INTEGER MIN               ! Number of minutes
      INTEGER MM                ! Month number
      CHARACTER*3 MONTHS(12)    ! List of all 12 months
      INTEGER SS                ! Number of seconds
      INTEGER YY                ! year number (2 or 4 digit)
      INTEGER LSTATUS

*  Local Date:
      DATA MONTHS /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     &     'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/

*-

      IF (STATUS .NE. 0) RETURN

*     Remove leading blanks
      CALL CHR_LDBLK ( DATE_OBS )

*     Find length of string
      ILEN = CHR_LEN ( DATE_OBS )

      print *,'ILEN = ',ILEN

*     Use length to guess at format
      IF (ILEN .EQ. 8) THEN

*     DD/MM/YY format
         READ( DATE_OBS, FMT=2221) DD, MM, YY

         READUT = .FALSE.

      ELSE IF (ILEN .EQ. 10) THEN

*     CCYY-MM-DD format
         READ( DATE_OBS, FMT=2222) YY, MM, DD

         READUT = .FALSE.

      ELSE IF (ILEN .EQ. 16) THEN

*     CCYY-MM-DD format
         READ( DATE_OBS, FMT=2225) YY, MM, DD, HH, MIN
         SS = 0
         FS = 0

         READUT = .TRUE.

      ELSE IF (ILEN .EQ. 19 .OR. ILEN .EQ. 20) THEN

*     "ccyy-mm-ddThh:mm:ss" or "ccyy-mm-ddThh:mm:ssZ"
         READ( DATE_OBS, FMT=2223) YY, MM, DD, HH, MIN, SS

         READUT = .TRUE.

      ELSE IF (ILEN .EQ. 23 .OR. ILEN .EQ. 24) THEN

*     "ccyy-mm-ddThh:mm:ss.sss" or "ccyy-mm-ddThh:mm:ss.sssZ"
         READ( DATE_OBS(:23), FMT=2224) YY, MM, DD, HH, MIN, SS, FS

         READUT = .TRUE.

      ELSE

*     Incorrect length
         STATUS = 1
         RETURN

      END IF

*     Sort out the return strings

*     Set the UT string
      IF ( READUT ) THEN

*     Insert the hours, minutes and seconds into the string
         WRITE (ITIME, '(I2.2,'':'',I2.2,'':'',I2.2)')
     &        HH, MIN, SS

      END IF

*     Create the date string
      WRITE (IDATE, '(I2.2,''-'',A3,''-'',I2.2)')
     &     DD, MONTHS(MM), MOD(YY,100)

*     Formats for parsing
 2221 FORMAT(I2,1X,I2,1X,I2)  ! DD/MM/YY
 2222 FORMAT(I4,1X,I2,1X,I2)  ! CCYY-MM-DD
 2223 FORMAT(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2) ! CCYY-MM-DDTHH:MM:SS[Z]
 2224 FORMAT(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2,1X,I3) ! CCYY-MM-DDTHH:MM:SS.SSS[Z]
 2225 FORMAT(I4,1X,I2,1X,I2,1X,I2,1X,I2) ! CCYY-MM-DDTHH:MM

      END


