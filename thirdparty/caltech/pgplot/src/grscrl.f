C GRSCRL -- scroll pixels in viewport
C+
      SUBROUTINE GRSCRL (DX, DY)
      INTEGER DX, DY
C
C Shift the pixels in the viewport by DX and DY in device coordinates.
C--
C 24-Feb-97: new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER NBUF, LCHR
      REAL RBUF(6)
      CHARACTER*8 CHR
C
C Do nothing if device is not open or not in appropriate state.
C
      IF (GRCIDE.LT.1) RETURN
      IF (.NOT.GRPLTD(GRCIDE)) RETURN
C
C If device has scroll capability, use it. The arguments in
C RBUF are: (1..4) current viewport in device coordinates; 
C (5..6) scroll displacement in world coordinates.
C
      IF (GRGCAP(GRCIDE)(11:11).EQ.'S') THEN
         RBUF(1) = NINT(GRXMIN(GRCIDE))
         RBUF(2) = NINT(GRYMIN(GRCIDE))
         RBUF(3) = NINT(GRXMAX(GRCIDE))
         RBUF(4) = NINT(GRYMAX(GRCIDE))
         RBUF(5) = DX
         RBUF(6) = DY
         NBUF = 6
         LCHR = 0
         CALL GREXEC(GRGTYP,30,RBUF,NBUF,CHR,LCHR)
C
C Otherwise, report an error.
C
      ELSE
         CALL GRWARN('Device does not support scrolling')
      END IF
      END
