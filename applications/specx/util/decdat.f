*  History:
*     19 Nov 1993 (hme):
*        Remove TABs.
C-----------------------------------------------------------------------

      SUBROUTINE DECDAT (IDATEC, JDATE)

C  Routine to decode the date from a character expression

      REAL*4    AMONTH(12)
      INTEGER   JDATE(3)
      CHARACTER IDATEC*9

      DATA AMONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     &           'JUL','AUG','SEP','OCT','NOV','DEC'/

      READ(IDATEC,10,ERR=99) JDATE(1),AMON,IYEAR
   10 FORMAT(I2,X,A3,X,I2)
      DO I=1,12
        IF(AMON.EQ.AMONTH(I)) GO TO 30
      END DO
   30 JDATE(2)=I
      JDATE(3)=1900+IYEAR

   99 RETURN
      END


