      SUBROUTINE MSC_DAYN(DAY, MONTH, YEAR, DATE, STATUS)

*+
*
*   Name:
*      SUBROUTINE MSC_DAYN
*
*   Description:
*      Convert YEAR, MONTH and DAY into DATE. IUE day number is given from
*      1 JAN 1978.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     15-JUL-94     IUEDR Vn. 3.1-1
*         Modified to support leap-years
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER DAY          ! day in month
      INTEGER MONTH        ! month index
      INTEGER YEAR         ! year A.D.

*   Export:
      INTEGER DATE         ! date from 1 JAN 1978
      INTEGER STATUS       ! status return

*   Local variables:
      INTEGER DAYS(12)     ! days in each month
      INTEGER I            ! loop index

*   Initialisations:
      DATA DAYS/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

      IF (MONTH.LT.1 .OR. MONTH.GT.12) THEN
         CALL ERROUT('Error: invalid month index\\', STATUS)
         RETURN
      ELSE IF (DAY.LT.1 .OR. DAY.GT.DAYS(MONTH)) THEN
         CALL ERROUT('Error: invalid day number\\', STATUS)
         RETURN
      ELSE IF (YEAR .LT. 1978) THEN
         CALL ERROUT('Error: year is pre-launch\\', STATUS)
         RETURN
      END IF

*   Extra day in feb in leap years
      IF (((YEAR + 3) / 4) .EQ. (YEAR / 4)) THEN
         DAYS(2) = 29
      END IF

      DATE = (YEAR - 1978) * 365 + DAY
      IF (MONTH.GT.1) THEN
         DO I = 1, MONTH - 1
            DATE = DATE + DAYS(I)
         END DO
      END IF
      END
