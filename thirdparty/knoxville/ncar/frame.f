      SUBROUTINE FRAME
C
C FRAME is intended to advance to a new frame.  The GKS version clears
C all open workstations.
C
C First, flush the PLOTIT buffer.
C
      CALL PLOTIT (0,0,2)
C
C Get the number of open workstations.  If there are none, we're done.
C
      CALL GQOPWK (0,IE,NO,ID)
      IF (NO.EQ.0) RETURN
C
C Otherwise, clear the open workstations.
C
      DO 101 I=1,NO
        CALL GQOPWK (I,IE,NO,ID)
        CALL GCLRWK (ID,1)
  101 CONTINUE
C
C Done.
C
      RETURN
C
      END
