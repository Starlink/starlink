C
C
C
      SUBROUTINE HSTMED(DATA,NPTS,W,MED)
C
C  FINDS MEDIAN OF ALL DATA POINTS USING SHELL SORT
C
      DIMENSION DATA(NPTS), W(NPTS)
      INTEGER GAP,I,J,JG,K,HALFN
      REAL MED
C
C  COPY DATA ARRAY TO WORK ARRAY
C
      DO 50 I = 1,NPTS
 50     W(I) = DATA(I)
C
C  DO SHELL SORT
C
      GAP = NPTS / 2
      HALFN = GAP
C
      DO 100 I = GAP + 1, NPTS
        IF (GAP .LE. 0) GOTO 900
        J = I - GAP
 200    CONTINUE
        IF (J .LE. 0) GOTO 300
        JG = J + GAP
        IF (W(J) .LE. W(JG)) THEN
          J = 0
        ELSE
          K = W(J)
          W(J) = W(JG)
          W(JG) = K
        ENDIF
        J = J - GAP
        GOTO 200
 300  GAP = GAP / 2
 100  CONTINUE
C
 900  IF (FLOAT(NPTS)/2. .GT. FLOAT(HALFN)) THEN
          MED = W(MIN0(HALFN + 1,NPTS))
      ELSE
          MED = (W(MAX0(1,HALFN)) + W(MIN0(HALFN + 1,NPTS)))/2.
      ENDIF
      RETURN
      END
