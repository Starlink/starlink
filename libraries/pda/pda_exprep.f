
      FUNCTION  PDA_EXPREP(RDUM)
C  This function replaces exp to avoid under- and overflows and is
C  designed for IBM 370 type machines. It may be necessary to modify
C  it for other machines. Note that the maximum and minimum values of
C  PDA_EXPREP are such that they has no effect on the algorithm.

      DOUBLE PRECISION  RDUM, PDA_EXPREP

      IF (RDUM .GT. 174.) THEN
         PDA_EXPREP = 3.69D+75
      ELSE IF (RDUM .LT. -180.) THEN
         PDA_EXPREP = 0.0
      ELSE
         PDA_EXPREP = EXP(RDUM)
      END IF

      RETURN
      END
