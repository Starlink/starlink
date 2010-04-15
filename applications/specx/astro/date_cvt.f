*  History:
*     22 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Dec 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*-----------------------------------------------------------------------

      SUBROUTINE DATE_CVT (INDATE, OUTDATE)

*  Routine to change date format from VAX format to nn/nn/nn form

      IMPLICIT   NONE

*     Formal parameters

      CHARACTER  INDATE*9
      CHARACTER  OUTDATE*8

*     Local variables

      INTEGER    I
      CHARACTER  TEMP*3

*     Data

      CHARACTER  MONTHS(12)*3
      DATA MONTHS /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     &                        'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/


*  OK, go...

      TEMP = INDATE(4:6)
      CALL UUCASE (TEMP)

      I = 1
      DO WHILE (TEMP.NE.MONTHS(I))
        I = I + 1
      END DO

      WRITE (OUTDATE, '(A2,''/'',I2.2,''/'',A2)')
     &        INDATE(1:2), I, INDATE(8:9)

      RETURN
      END

*-----------------------------------------------------------------------

