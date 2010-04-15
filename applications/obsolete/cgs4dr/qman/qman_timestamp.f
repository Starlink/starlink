*+  QMAN_TIMESTAMP - Provides timestamps for QMAN task
      SUBROUTINE QMAN_TIMESTAMP( STATUS )
*    Invocation :
*     CALL QMAN_TIMESTAMP( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*    History :
*     27-May-1994: Original version                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'               ! Defines SAI__OK and others
      INCLUDE 'PRM_PAR'               ! Defines machine precision
      INCLUDE 'MESSYS_LEN'                 ! Defines MSG_VAL_LEN etc
*    Status :
      INTEGER STATUS                  ! Inherited global ADAM status
*    Global variables :
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN global parameter constants
*    Local variables :
      INTEGER NTICKS                  ! PoSiX time ticks
      INTEGER SECONDS                 ! Number of seconds pas the minute
      INTEGER MINUTES                 ! Number of minutes past the hour
      INTEGER HOURS                   ! Number of hours elapsed today
      INTEGER DAYS                    ! Number of days elapsed in month
      INTEGER MONTHS                  ! Number of month in year {0:11}
      INTEGER YEAR                    ! Year
      INTEGER IDY, YDY, SDY, PTR      ! PoSiX variables not used here
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Increment and reset the fraction as necessary
      IF ( FRACTION .GT. ACCURACY ) FRACTION = 1
      FRACTION = FRACTION + 1

*   Get a PoSiX time and convert it to local time
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_LOCALTIME( NTICKS, SECONDS, MINUTES, HOURS,
     :  DAYS, MONTHS, YEAR, IDY, YDY, SDY, PTR, STATUS )

*   Increment the month to be in range 1-12 rather than 0-11
      MONTHS = MONTHS + 1

*   Get the modified julian date using century defaults
      TIMESTAMP = 0.0
      CALL SLA_CALDJ( YEAR, MONTHS, DAYS, TIMESTAMP, STATUS )

*   Set the timestamp
      TIMESTAMP = TIMESTAMP + DBLE(HOURS)/HPD + DBLE(MINUTES)/MPD +
     :  DBLE(SECONDS)/SPD + DBLE(FRACTION)/FPD

*   Exit subroutine
      END
