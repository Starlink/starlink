*+  RED4_DAY_DIFFERENCE - Difference between two dates in days.
      SUBROUTINE RED4_DAY_DIFFERENCE( IDATE1, IDATE2, NDAYS, STATUS )
*    Description :
*     This routine determines the difference between two dates
*     of the form yyyymmdd in days.
*    Invocation :
*     CALL RED4_DAY_DIFFERENCE( IDATE1, IDATE2, NDAYS, STATUS )
*    Parameters :
*     IDATE1    = INTEGER( READ )
*         The earliest date, in the form yyyymmdd.
*     IDATE2    = INTEGER( READ )
*         The latest date, in the form yyyymmdd.
*     NDAYS     = INTEGER( WRITE )
*         The number of days between the two dates (IDATE2-IDATE1).
*     STATUS    = INTEGER( UPDATE )
*         Global ADAM status.
*    Method :
*     Each of the dates is converted into a Julian day number, and the
*     two numbers are subtracted.
*    Deficiencies :
*    Bugs :
*    Authors :
*     S.M.Beard    (REVAD::SMB)
*     P.N.Daly     (JACH::PND)
*    History :
*     30-Nov-1990: Original version.                       (SMB)
*     18-Feb-1993: Conform to error strategy               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Global variables:
*    Import :
      INTEGER
     :  IDATE1,               ! First date
     :  IDATE2                ! Second date
*    Export:
      INTEGER
     :  NDAYS                 ! Number of days
*    Status :
      INTEGER STATUS
*    External references:
*    Local Constants :
*    Local variables :
      INTEGER
     :  YEAR1, MONTH1, DAY1,  ! Year, month and day of IDATE1
     :  YEAR2, MONTH2, DAY2,  ! Year, month and day of IDATE2
     :  TMP                   ! Temporary storage for date
       DOUBLE PRECISION
     :  MJD1,                 ! Julian day number of IDATE1 (+0.5)
     :  MJD2                  ! Julian day number of IDATE2 (+0.5)
*    Local data :
*-

*    Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Split the first date into separate year, month and days.
      TMP = IDATE1

      YEAR1 = TMP / 10000
      TMP = TMP - (YEAR1 * 10000)

      MONTH1 = TMP / 100
      TMP = TMP - (MONTH1 * 100)

      DAY1 = TMP

*   Split the second date into separate year, month and days.
      TMP = IDATE2

      YEAR2 = TMP / 10000
      TMP = TMP - (YEAR2 * 10000)

      MONTH2 = TMP / 100
      TMP = TMP - (MONTH2 * 100)

      DAY2 = TMP

*   Convert both the dates into Julian day numbers (+0.5).
      CALL SLA_CALDJ( YEAR1, MONTH1, DAY1, MJD1, STATUS )
      CALL SLA_CALDJ( YEAR2, MONTH2, DAY2, MJD2, STATUS )

*   Subtract the two Julian day numbers to get the difference
*   between the two dates in days. (Note that the JD variables
*   differ from true Julian day number by 0.5 days, but this
*   factor cancels out in the subtraction).
      NDAYS = NINT( MJD2 - MJD1 )

      END
