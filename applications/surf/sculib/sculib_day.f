      DOUBLE PRECISION FUNCTION SCULIB_DAY ()
*+
*  Name:
*     SCULIB_DAY

*  Purpose:
*     returns date and time as day number since 1st Jan

*  Description:
*     Calculates the time since 1st January. The time is returned
*     as a day number and fractional day.

*  Invocation:
*     DAY = SCULIB_DAY ()

*  Arguments:
*     None

*  Returned Value:
*     SCULIB_DAY = DOUBLE PRECISION
*           date and time as a day number since 1st Jan

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1994,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:

*  History:
*     $Id$
*     10-OCT-1994: Original version.

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:

*  Arguments Given & Returned:

*  Arguments Returned:

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
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

*  Internal References:

*  Local data:

*.

      STATUS = SAI__OK

*  get time and date

      CALL PSX_TIME (NTICKS, STATUS)
      CALL PSX_LOCALTIME (NTICKS, SECS, MINS, HOURS, DAY, MONTH, YEAR,
     :  WDAY, YDAY, ISDST, TSTRCT, STATUS)

*  calculate day number

      SCULIB_DAY = DBLE (YDAY) + (((DBLE(HOURS) * 60.0D0) +
     :  DBLE(MINS)) * 60.0D0 + DBLE(SECS)) / (24.0D0 * 60.0D0 * 60.0D0)

      END

