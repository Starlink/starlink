*-----------------------------------------------------------------------

      SUBROUTINE FINDAZEL (IFAIL)

*  Routine to calculate and print zenith distance at which current
*  spectrum was observed.

*  History:
*     20-SEP-2000 (AJC):
*        Unused DECRAD, RARAD
*-

      IMPLICIT  NONE

*     Formal parameter(s):

      INTEGER   IFAIL

*     Include files

      INCLUDE   'STACKCOMM'
      INCLUDE   'FLAGCOMM'

*     Local variables

      DOUBLE PRECISION   HOUR_ANGLE
      DOUBLE PRECISION   UTD, JULIAN_DATE, SIDEREAL_TIME

      DOUBLE PRECISION   DPI
      DATA               DPI  /3.141592654/

*   Ok, go...

      CALL ASTRO_TIMES (ITIME, IDATE, ALONG, TIMCOR, IUTFLG,
     &                  UTD, SIDEREAL_TIME, JULIAN_DATE)

      HOUR_ANGLE = SIDEREAL_TIME*DPI/12. - RA*DPI/180.D0
      CALL HADEC_TO_AZEL   (HOUR_ANGLE, DEC*DPI/180.D0,
     &                      DPI*ALAT/180., AZ, EL)

      AZ = AZ * 180./DPI
      EL = EL * 180./DPI

      WRITE (ILOUT,1000) EL
      WRITE (ILOUT,1001) AZ
      RETURN

 1000 FORMAT(' Elevation of observation : ', F7.3, ' degrees')
 1001 FORMAT(' Azimuth of observation:    ', F7.2, ' degrees')

      END

*-----------------------------------------------------------------------

