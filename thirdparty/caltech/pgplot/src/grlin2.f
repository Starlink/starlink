C*GRLIN2 -- draw a normal line
C+
      SUBROUTINE GRLIN2 (X0,Y0,X1,Y1)
C
C GRPCKG : plot a visible line segment in absolute coords from
C (X0,Y0) to (X1,Y1).  The endpoints of the line segment are rounded
C to the nearest integer and passed to the appropriate device-specific
C routine. It is assumed that the entire line-segment lies within the
C view surface, and that the physical device coordinates are
C non-negative.
C--
C (1-Jun-1984)
C 19-Oct-1984 - rewritten for speed [TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    X0,Y0,X1,Y1
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER CHR
C
C- If this is first thing plotted then set something plotted flag
C- and for a GREXEC device call BEGIN_PICTURE.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C---
      RBUF(1)=X0
      RBUF(2)=Y0
      RBUF(3)=X1
      RBUF(4)=Y1
      NBUF=4
C     WRITE(*,'(A,4F10.5)') 'GRLIN2',RBUF(1), RBUF(2), RBUF(3), RBUF(4)
      CALL GREXEC(GRGTYP,12,RBUF,NBUF,CHR,LCHR)
C
      END
