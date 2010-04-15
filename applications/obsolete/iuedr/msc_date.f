      SUBROUTINE MSC_DATE(DATE, DAY, MONTH, YEAR, STATUS)

*+
*
*   Name:
*      SUBROUTINE MSC_DATE
*
*   Description:
*      Convert DATE into YEAR, MONTH, DAY.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     15-JUL-94     IUEDR Vn. 3.1-1
*         Added support for leap-years
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER DATE         ! date from 1 JAN 1978

*   Export:
      INTEGER DAY          ! day in month
      INTEGER MONTH        ! month index
      INTEGER YEAR         ! year A.D.
      INTEGER STATUS       ! status return

*   Local variables:
      INTEGER DAYC         ! day count in year
      INTEGER DAYN         ! day number in year
      INTEGER DAYS(12)     ! days in each month
      INTEGER LEAPS        ! leap days since launch
      INTEGER I            ! loop index

*   Initialisations:
      DATA DAYS/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

*   YEAR
      YEAR = DATE / 365 + 1978
      LEAPS = (DATE + 365) / 1461
      DAYN = DATE - (YEAR - 1978) * 365 - LEAPS
*   Give extra day to Feb in leap year
      IF (((YEAR + 3) / 4) .EQ. (YEAR / 4)) THEN
         DAYS(2) = 29
      END IF

*   MONTH
      DAYC = 0
      MONTH = 0

      DO I = 1, 12
         IF (DAYS(I) + DAYC.GT.DAYN) THEN
            MONTH = I
            GO TO 200
         ELSE
            DAYC = DAYC + DAYS(I)
         END IF
      END DO

*   DAY
 200  CONTINUE
      DAY = DAYN - DAYC
      STATUS = 0
      END
