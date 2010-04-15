      INTEGER FUNCTION FIND31( IDAY, IMONTH, IYEAR )
*+
*  Name:
*     FIND31

*  Purpose:
*     To calculate modified julian date

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = FIND31( IDAY, IMONTH, IYEAR )

*  Description:
*     To calculate Modified Julian Date

*  Arguments:
*     IDAY = INTEGER (Given)
*        Day of date for which modified julian date is to be calculated
*     IMONTH = INTEGER (Given)
*        Month of date for which modified julian date is to be calc'd
*     IYEAR = INTEGER (Given)
*        Year of date for which modified julian date is to be calculated

*  Returned Value:
*     FIND31 = INTEGER
*        Modified Julian date

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     20-JAN-1992 (DCP):
*        Original version.
*        This original version is adapted from MJD, a subroutine
*        of POSNTIM, contained in its utilities subdirectory.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER IDAY
      INTEGER IMONTH
      INTEGER IYEAR

*  Local Variables:
      INTEGER BASEJD             ! Base Julian date for 1st Jan 1900
      INTEGER CENT19             ! Number of centuries since 1900
      INTEGER CENTYR             ! Number of centuries entered in year
      INTEGER JDPCEN             ! Julian days per century
      INTEGER JUDAY              ! The number of days since the start of
                                 ! the year
      INTEGER JUYEAR             ! The number of Julian days up to the
                                 ! begining of the year
      INTEGER LEAP4              ! Is 0 if the year is a multiple of 4
      INTEGER MTHDAY( 12 )       ! Number of days between the start of
                                 ! the year and the start of the month
      INTEGER YEARSC             ! Number of years since the begining of
                                 ! the century
      INTEGER YRTODA             ! Number of days in the year

*  Local Data:
      DATA BASEJD / 15019 /
      DATA JDPCEN / 36524 /
      DATA MTHDAY / 0,31,59,90,120,151,181,212,243,273,304,334 /
      DATA YRTODA / 365 /
*.

*  Calculate the number of days since the begining of the year without
*  leap year correction
      JUDAY = MTHDAY( IMONTH ) + IDAY

*  Calculate whether the year is a multiple of 4
      LEAP4 = MOD( IYEAR, 4)

*  Calculate whether the year is a multiple of 100
      YEARSC = MOD( IYEAR, 4)

*  Add 1 day for leap year if year is divisible by 4 but not by 100 and
*  the month is more than February
      IF ( ( LEAP4 .EQ. 0 ) .AND. ( YEARSC .NE. 0 )) THEN
         IF ( IMONTH .GT. 2 ) JUDAY = JUDAY + 1
      END IF

*  Calculate the number of Julian days in the years between 1900 and the
*  first day of the given year (without correction for intervening leap
*  years)
      JUYEAR = YEARSC * YRTODA

*  Modify this for the number of intervening leap years
      JUYEAR = JUYEAR + INT( ( YEARSC - 1 ) / 4)

*  Calculate the Julian date as:- Base Julian Date for 1900 +
*  Julian days 1900 to begining of year + Julian days begining of year
*  to current date
      FIND31 = BASEJD + JUYEAR + JUDAY

*  Check whether the year has been given as eg 1983 or 83
*  by calculating IYEAR divided by 100
      CENTYR = INT( IYEAR / 100)

*  If this is non zero we need to correct for the number of centuries
*  different form 1900
      IF ( CENTYR .NE. 0 ) THEN
         CENT19 = CENTYR - 19

*  Correct julian date for number of centuries since 1900
         FIND31 = FIND31 + CENT19 * JDPCEN
      END IF

*  Return the modified julian date value
      RETURN

      END
