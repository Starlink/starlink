*+  SCULIB_DAY - returns date and time as day number since 1st Jan
      DOUBLE PRECISION FUNCTION SCULIB_DAY ()
*    Description :
*    Invocation :
*     DAY = SCULIB_DAY ()
*    Parameters :
*    Result :
*     SCULIB_DAY = DOUBLE PRECISION 
*           date and time as a day number since 1st Jan
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     10-OCT-1994: Original version.
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER DAY                         ! number of day in month
      INTEGER HOURS                       ! hours
      INTEGER ISDST                       ! daylight saving flag
      INTEGER MINS                        ! minutes
      INTEGER MONTH                       ! number of month in year
      INTEGER NTICKS                      ! ?
      INTEGER SECS                        ! seconds
      INTEGER STATUS                      ! local status
      INTEGER TSTRCT                      ! not used
      INTEGER WDAY                        ! number of day in week
      INTEGER YDAY                        ! number of day in year
      INTEGER YEAR                        ! last 2 digits of year
*    Internal References :
*    Local data :
*-

      STATUS = SAI__OK

*  get time and date

      CALL PSX_TIME (NTICKS, STATUS)
      CALL PSX_LOCALTIME (NTICKS, SECS, MINS, HOURS, DAY, MONTH, YEAR,
     :  WDAY, YDAY, ISDST, TSTRCT, STATUS)

*  calculate day number

      SCULIB_DAY = DBLE (YDAY) + (((DBLE(HOURS) * 60.0D0) + 
     :  DBLE(MINS)) * 60.0D0 + DBLE(SECS)) / (24.0D0 * 60.0D0 * 60.0D0)

      END

