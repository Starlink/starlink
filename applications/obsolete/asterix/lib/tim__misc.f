*+  TIM_MJDUTC2TAI - Convert MJD,UTC to atomic time (TAI)
      SUBROUTINE TIM_MJDUTC2TAI( BASE_MJD, BASE_UTC, BASE_TAI )
*
*    Description :
*
*     Converts an MJD supplied as an integer day plus seconds offset
*     to base atomic time. Takes leap seconds into account.  See the
*     ASTERIX standards document for definitions of these quantities.
*     Variables and arguments have been named to match those in the
*     documentation.
*
*    History :
*
*     24 Jul 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      INTEGER                BASE_MJD                  ! Days since whenever
      DOUBLE PRECISION       BASE_UTC                  ! Seconds since day start
*
*    Export :
*
      DOUBLE PRECISION       BASE_TAI                  ! Atomic time
*
*    Functions :
*
      DOUBLE PRECISION       SLA_DAT                   ! Leap seconds routine
*
*    Local constants :
*
      DOUBLE PRECISION       LEAPS_AT_1970
        PARAMETER            ( LEAPS_AT_1970 = 10.0D0 )
      DOUBLE PRECISION       MJD_AT_1970
        PARAMETER            ( MJD_AT_1970 = 41317.0D0 )
      DOUBLE PRECISION       SECONDS_IN_DAY
        PARAMETER            ( SECONDS_IN_DAY = 86400.0D0 )
*
*    Local variables :
*
      DOUBLE PRECISION       OBS_MJD                   ! Combined MJD and UTC
*-

*    Find observation start
      OBS_MJD = DBLE( BASE_MJD ) + BASE_UTC / SECONDS_IN_DAY

*    The atomic time in days
      BASE_TAI = OBS_MJD +
     :     ((SLA_DAT(OBS_MJD)-LEAPS_AT_1970)/SECONDS_IN_DAY) -
     :     MJD_AT_1970

      END
