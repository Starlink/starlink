
C--------------------------------------------------------------------------

      SUBROUTINE GEN_MATCH (STRING1, STRING2, NWRD1, IMATCH)

C   Routine to establish if commands in STRING1 match those in STRING2.
C   It is assumed that the number of words in STRING2 is known to be less
C   than or equal to the number if STRING1.

      CHARACTER STRING1*(*),STRING2*(*),WORD1*40,WORD2*40

      ILC    = MIN0 ( LEN(STRING1), LEN(STRING2) )
      IMATCH = 0
      DO 100 NWRD = 1,NWRD1
      CALL GEN_GETWRD (STRING1, NWRD, LWRD1, WORD1)
      CALL GEN_GETWRD (STRING2, NWRD, LWRD2, WORD2)
      IF (LWRD2.LT.LWRD1)   RETURN
      IF (WORD1(1:LWRD1).NE.WORD2(1:LWRD1)) RETURN
  100 CONTINUE

      IMATCH = 1

      RETURN
      END
