*-----------------------------------------------------------------------

      SUBROUTINE HOURS_TO_STRING (HOURS, STRING)

*  Routine to convert time in hours to string nn:nn:nn.nnn

      IMPLICIT   NONE

*     Formal parameters

      DOUBLE PRECISION  HOURS
      CHARACTER         STRING*12

*     Local variables

      INTEGER           ITEMP(4)

*  Ok, go...

      CALL DEG_TO_DMS (HOURS, ITEMP)
      WRITE (STRING, '(I2.1,'':'',I2.2,'':'',I2.2,''.'',I2.2,''0'')')
     &       ITEMP(1), ITEMP(2), ITEMP(3), ITEMP(4)

      RETURN
      END

*-----------------------------------------------------------------------

