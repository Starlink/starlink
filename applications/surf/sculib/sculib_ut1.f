*+  SCULIB_UT1 - returns UT1 as a modified Julian day (calculated from
*                Hawaiian local time)
      DOUBLE PRECISION FUNCTION SCULIB_UT1 ()
*    Description :
*    Invocation :
*     UT1 = SCULIB_UT1
*    Parameters :
*    Result :
*     UT1 = DOUBLE PRECISION 
*           UT1 expressed as a modified Julian day (JD - 2400000.5)
*    Method :
*    Deficiencies :
*     Depends on VAX clock for local time and date
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
*    External references :
      REAL SECNDS                        ! VMS SECNDS function
*    Global variables :
*    Local Constants :
      DOUBLE PRECISION OBS_TIME_ZONE                ! observatory time zone (hrs)
      PARAMETER       (OBS_TIME_ZONE = 10.0D0)      ! Hawaii
*    Local variables :
      INTEGER SLA_STATUS
      INTEGER ID, IM, IY
      DOUBLE PRECISION DJM               ! modified Julian day
*    Internal References :
*    Local data :
*-

*  get the VAX date
 
      CALL IDATE (IM, ID, IY)

*  calculate the modified Julian date for 0h local time

      CALL SLA_CALDJ (IY, IM, ID, DJM, SLA_STATUS)

*  add time zone to get Julian date for 0h UT

      DJM = DJM + OBS_TIME_ZONE / 24.0D0

*  and add number of seconds since midnight onto DJM to give UT1

      SCULIB_UT1 = DJM + DBLE (SECNDS(0.0)) / (3600.0D0 * 24.0D0)

      END

