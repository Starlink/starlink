*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
      SUBROUTINE INCXY(LIM,INC)
      REAL LIM(2), TEMP
      INTEGER INC
      IF ( LIM(1).LT.LIM(2) ) THEN
        INC=0
      ELSE
        INC=1
        TEMP=LIM(1)
        LIM(1)=LIM(2)
        LIM(2)=TEMP
      END IF
      END
