C*GRSETS -- change size of view surface
C+
      SUBROUTINE GRSETS (IDENT,XSIZE,YSIZE)
C
C GRPCKG : change size of plotting area. The requested dimensions
C will be reduced to the absolute maximum of the plot device if
C necessary.
C
C Arguments:
C
C IDENT (input, integer): plot identifier from GROPEN.
C XSIZE (input, real): new x dimension of plot area (absolute
C               units); if less than zero, the default dimension
C               will be used.
C YSIZE (input, real): new y dimension of plot area (absolute
C               units); if less than zero, the default dimension
C               will be used.
C--
C (1-Feb-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C  5-Jan-1993 - set GRADJU [TJP].
C------------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I, IDENT, J, IX, IY, NBUF,LCHR
      REAL     RBUF(6)
      CHARACTER CHR
      REAL     XSIZE,YSIZE
C
      CALL GRSLCT(IDENT)
C     write (*,*) 'GRSETS: old size', GRXMXA(IDENT), GRYMXA(IDENT)
      CALL GRPAGE
      IF ((XSIZE .LT. 0.0) .OR. (YSIZE .LT. 0.0)) THEN
          CALL GREXEC(GRGTYP, 6,RBUF,NBUF,CHR,LCHR)
          GRXMXA(IDENT) = RBUF(2)
          GRYMXA(IDENT) = RBUF(4)
      ELSE
          I = NINT(XSIZE)
          J = NINT(YSIZE)
          CALL GREXEC(GRGTYP, 2,RBUF,NBUF,CHR,LCHR)
          IX=RBUF(2)
          IY=RBUF(4)
          IF (IX.GT.0) I = MIN(I,IX)
          IF (IY.GT.0) J = MIN(J,IY)
          GRXMXA(IDENT) = I
          GRYMXA(IDENT) = J
      END IF
C     write (*,*) 'GRSETS: new size', GRXMXA(IDENT), GRYMXA(IDENT)
      GRXMIN(IDENT) = 0
      GRXMAX(IDENT) = GRXMXA(IDENT)
      GRYMIN(IDENT) = 0
      GRYMAX(IDENT) = GRYMXA(IDENT)
      GRADJU(IDENT) = .TRUE.
C
      END
