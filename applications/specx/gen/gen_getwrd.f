
C------------------------------------------------------------------------

      SUBROUTINE GEN_GETWRD (STRING, NWRD, LWRD, WORD)

C  Routine to get word(NWRD) from string STRING1, where the words
C  are considered to be divided by the hyphen -. The number of characters
C  is returned in LWRD and the word itself in WORD.

      CHARACTER STRING*(*),WORD*(*)

      ILS=LEN(STRING)
      ILW=LEN(WORD)
      WORD=' '
      LWRD=0
      IWRD=0
      I=0
   10 IWRD=IWRD+1
      IFIN=I+1
      IF(IWRD.EQ.NWRD)   GO TO 100
      DO I=IFIN,ILS
        IF(STRING(I:I).EQ.'-')   GO TO 10
      END DO

  100 IST=I+1
      DO I=IST,ILS
        IF(STRING(I:I).EQ.'-'.OR.STRING(I:I).EQ.' ')  RETURN
        LWRD=LWRD+1
        IF(LWRD.GT.ILW)   RETURN
        WORD(I+1-IST:I+1-IST)=STRING(I:I)
      END DO

      RETURN
      END
