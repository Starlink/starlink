      DOUBLE PRECISION FUNCTION SCULIB_UT1 ()
*+
*  Name:
*     SCULIB_UT1

*  Purpose:
*     returns UT1 as a modified Julian day

*  Description:
*     Return the current modified Julian day calculated for Hawaiian
*     local time.

*  Invocation:
*     UT1 = SCULIB_UT1

*  Arguments:
*     None

*  Returned Value:
*     SCULIB_UT1 = DOUBLE PRECISION 
*        UT1 expressed as a modified Julian day (JD - 2400000.5)


*  Notes:
*     - Depends on system clock for local time and date
*     - Uses the IDATE function to retrieve the current date. This 
*     only returns a 2-digit year (not Y2K compliant) but is not
*     a problem since the output is immediately passed to the SLALIB
*     routine SLA_CALDJ which uses windowing to select the correct
*     year.

*  Bugs:
*     The time zone is hard-wired for Hawaii (10).

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Id$

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
      REAL SECNDS                        ! VMS SECNDS function

*  Global variables:

*  Local Constants:
      DOUBLE PRECISION OBS_TIME_ZONE                ! observatory time zone (hrs)
      PARAMETER       (OBS_TIME_ZONE = 10.0D0)      ! Hawaii

*  Local variables:
      INTEGER SLA_STATUS
      INTEGER ID, IM, IY
      DOUBLE PRECISION DJM               ! modified Julian day

*  Internal References:

*  Local data:

*.

*  get the VAX date
 
      CALL IDATE (IM, ID, IY)

*  calculate the modified Julian date for 0h local time

      CALL SLA_CALDJ (IY, IM, ID, DJM, SLA_STATUS)

*  add time zone to get Julian date for 0h UT

      DJM = DJM + OBS_TIME_ZONE / 24.0D0

*  and add number of seconds since midnight onto DJM to give UT1

      SCULIB_UT1 = DJM + DBLE (SECNDS(0.0)) / (3600.0D0 * 24.0D0)

      END

