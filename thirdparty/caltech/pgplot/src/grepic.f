C*GREPIC -- end picture
C+
      SUBROUTINE GREPIC
C
C GRPCKG: End the current picture.
C
C Arguments: none.
C--
C 17-Nov-1994 - [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER CHR
C
C Check a plot is open.
C
      IF (GRCIDE.LT.1) RETURN
C
C End picture.
C
      IF (GRPLTD(GRCIDE)) THEN
            RBUF(1) = 1.
            NBUF = 1
            CALL GREXEC(GRGTYP,14,RBUF,NBUF,CHR,LCHR)
      END IF
      GRPLTD(GRCIDE) = .FALSE.
C
      END
