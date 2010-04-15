*+STR2MJD   Converts string date to MJD
      SUBROUTINE STR2MJD(DSTRING,MJD)
      IMPLICIT NONE

*   Input :
      CHARACTER*11 DSTRING
*   Output :
      DOUBLE PRECISION MJD


*  History
*     1992 DEC		M DUESTERHAUS   CREATE ORIGINAL
****************************************************************************-
*   Functions :
      INTEGER MDH_CTOI


*   LOCAL
      INTEGER YEAR,MONTH, DAY, STATUS, I
      CHARACTER*3 MNAME(12)
      DATA MNAME /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
     &            'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/


      YEAR = MDH_CTOI(DSTRING(1:4))
      DAY = MDH_CTOI (DSTRING(10:11))
      DO I=1,12
        IF (DSTRING(6:8).EQ.MNAME(I)) THEN
          MONTH = I
        END IF
      END DO

      CALL SLA_CALDJ(YEAR, MONTH, DAY, MJD, STATUS)
      END
